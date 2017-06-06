{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module C4D.SLCAN where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Base
import Ivory.Tower.HAL.Bus.CAN

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.CAN.Peripheral

import C4D.Platforms

-- http://elixir.free-electrons.com/linux/v4.11.3/source/drivers/net/can/slcan.c

slCANTower :: ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Struct "can_message")
           -> ChanOutput ('Struct "can_message")
           -> ClockConfig
           -> TestCAN
           -> Tower p ()
slCANTower ostream istream canctl canres cc can = do
  towerDepends canDriverTypes
  towerModule  canDriverTypes

  towerDepends basecanTypes
  towerModule  basecanTypes

  monitor "slcan" $ do
    rtr <- stateInit "rtr" (ival false)
    ext <- stateInit "ext" (ival false)
    idcnt <- stateInit "idcnt" (ival (0 :: Uint8))
    dlc <- stateInit "dlc" (ival (0 :: Uint8))
    expectDlc <- stateInit "expectDlc" (ival (0 :: Uint8))
    canid <- stateInit "canid" (ival (0 :: Uint32))
    canspeed <- stateInit "canspeed" (ival (0 :: Uint8))

    canframe <- state "canframe"

    transInit <- stateInit "transInit" (ival true)
    transID <- stateInit "transID" (ival false)
    transDLC <- stateInit "transDLC" (ival false)
    transData <- stateInit "transData" (ival false)
    transDone <- stateInit "transDone" (ival false)
    transSpeed <- stateInit "transSpeed" (ival false)
    transEOF <- stateInit "transEOF" (ival false)
    initParseFail <- stateInit "initParseFail" (ival false)

    failcount <- stateInit "failcount" (ival (0 :: Uint32))
    halfinput <- state "halfinput"
    failchar <- state "failchar"
    ignore <- stateInit "ignore" (ival false)

    sent <- stateInit "slcan_sent_count" (ival (0 :: Uint32))

    handler istream "istream" $ do
      c <- emitter canctl 1
      callbackV $ \input -> do
        let testChar = (input `isChar`)

        tinit <- deref (transInit)
        tid <- deref (transID)
        tdlc <- deref (transDLC)
        tdata <- deref (transData)
        teof <- deref (transEOF)
        tspeed <- deref (transSpeed)

        when tinit $ do
          emptyframe <- local $ istruct []
          refCopy canframe emptyframe

          store ignore false
          store initParseFail false

          cond_
            [ testChar 'T' ==> do
                store rtr false
                store ext true
            , testChar 't' ==> do
                store rtr false
                store ext false
            , testChar 'R' ==> do
                store rtr true
                store ext true
            , testChar 'r' ==> do
                store rtr true
                store ext false
            , testChar 'S' ==> do -- can speed
                store transInit false
                store transSpeed true
            -- open/close
            , testChar 'o' ==> store ignore true
            , testChar 'O' ==> store ignore true
            , testChar 'c' ==> store ignore true
            , testChar 'C' ==> store ignore true
            , testChar '\r' ==> store ignore true
            , true ==> do
                store initParseFail true
                fc <- deref failcount
                when (fc ==? 0) $ store failchar input
                failcount %= (+1)
            ]

          pfailed <- deref initParseFail
          ig <- deref ignore
          ts <- deref transSpeed
          -- unless this is ignored byte, speed config or parsing failure
          -- transition to id parsing
          unless (ig .|| ts .|| pfailed) $ do
            isExt <- deref ext
            ifte_ isExt (store idcnt 8) (store idcnt 3)

            store transInit false
            store transID true

        when tspeed $ do
          binInput <- call toBin input
          store canspeed binInput
          let canReinit baud = canInit (testCAN can) baud (testCANRX can) (testCANTX can) cc

          cond_ [ testChar '0' ==> canReinit 10000
                , testChar '1' ==> canReinit 20000
                , testChar '2' ==> canReinit 50000
                , testChar '3' ==> canReinit 100000
                , testChar '4' ==> canReinit 125000
                , testChar '5' ==> canReinit 250000
                , testChar '6' ==> canReinit 500000
                --, testChar '7' ==> canReinit 800000 --XXX: won't pass legal timings test
                , testChar '8' ==> canReinit 1000000
                , true         ==> canReinit 1000000
                ]

          store transSpeed false
          store transInit true

        when tid $ do
          cid <- deref canid
          binInput <- call toBin input
          store canid $ (cid `iShiftL` 4) + (safeCast binInput)

          pid <- deref idcnt
          store idcnt (pid - 1)
          ccid <- deref idcnt
          when (ccid ==? 0) $ do
            store transID false
            store transDLC true

        when tdlc $ do
          binInput <- call toBin input
          store dlc binInput
          store expectDlc (binInput*2)

          ifte_ (binInput /=? 0)
            (do
                store transDLC false
                store transData true
            )
            (do
                store transDLC false
                store transDone true
            )

        when tdata $ do
          cdlc <- deref dlc
          cex <- deref expectDlc
          store expectDlc (cex - 1)

          binInput <- call toBin input

          ifte_ (cex .% 2 ==? 0)
            (do
                store halfinput (binInput `iShiftL` 4)
            )
            (do
                hi <- deref halfinput
                idx <- assign $ toIx $ (cdlc - 1) - (cex `iDiv` 2)
                store (canframe ~> can_message_buf ! idx) (hi + binInput)
            )

          ex <- deref expectDlc
          when (ex ==? 0) $ do
            store transData false
            store transEOF true

        when teof $ do
            -- if this assert fires we're getting UART RX buffer overruns
            -- (can be checked in gdb with: p uart2_rx_overruns)
            -- and this is bad as you need to throttle input rate to SLCAN
            assert (testChar '\r')
            store transEOF false
            store transDone true

        tdone <- deref transDone
        when tdone $ do
          isExt <- deref ext
          ifte_ isExt
            (do
                cid <- deref canid
                isRtr <- deref rtr
                let msgid = extendedCANID (fromRep cid) (boolToBit isRtr)
                store (canframe ~> can_message_id) msgid
            )
            (do
                cid <- deref canid
                isRtr <- deref rtr
                let msgid = standardCANID (fromRep $ bitCast cid) (boolToBit isRtr)
                store (canframe ~> can_message_id) msgid
            )

          cdlc <- deref dlc
          store (canframe ~> can_message_len) (toIx cdlc)
          emit c (constRef canframe)
          sent += 1

          store transDone false
          store transInit true

    received <- stateInit "slcan_received_count" (ival (0 :: Uint32))
    lastrecv <- state "slcan_lastrecv"
    lastid <- state "slcan_lastid"

    handler canres "result" $ do
      o <- emitter ostream 32
      callback $ \msg -> do
        received += 1

        refCopy lastrecv msg

        canid' <- msg ~>* can_message_id
        canlen <- msg ~>* can_message_len

        isExtended <- assign $ bitToBool (canid' #. can_arbitration_ide)
        isRtr <- assign $ bitToBool (canid' #. can_arbitration_rtr)
        cid <- assign $ canid' #. can_arbitration_id

        let putStdID :: (GetAlloc eff ~ 'Scope s) => Ivory eff ()
            putStdID = do
              i <- standardIDToArray (toRep cid)
              i0 <- deref (i ! 0)
              i1 <- deref (i ! 1)
              putHex' o i0
              putHex  o i1

        ifte_ isRtr
          (do
              ifte_ isExtended
                (do
                    puts o "R"
                    i <- extendedIDToArray (toRep cid)
                    putHexArray o i
                )
                (do
                    puts o "r"
                    putStdID
                )
          )
          (do
              ifte_ isExtended
                (do
                    puts o "T"
                    store lastid $ toRep cid
                    i <- extendedIDToArray (toRep cid)
                    putHexArray o i
                )
                (do
                    puts o "t"
                    putStdID
                )
          )

        -- needs ixToUint8 or something
        putc o (((bitCast :: Uint32 -> Uint8) $ signCast $ fromIx canlen) + (fromIntegral $ ord '0'))
        putHexArray o (msg ~> can_message_buf)
        puts o "\r"
