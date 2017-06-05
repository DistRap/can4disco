{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module C4D.Tests.SLCANLoopback where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import Ivory.BSP.STM32.Peripheral.CAN.Peripheral

import GHC.TypeLits
import C4D.Platforms
import BSP.Tests.LED
import BSP.Tests.UART.Buffer
import BSP.Tests.UART.Types

puts :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> String -> Ivory eff ()
puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

putc :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putc = emitV

putHex :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putHex e val = do
  let hi = (val .& 0xF0) `iShiftR` 4 + (fromIntegral $ ord '0')
      lo = (val .& 0x0F) + (fromIntegral $ ord '0')

  ifte_ (hi >=? 0x3a) (emitV e $ hi + 7) (emitV e hi)
  ifte_ (lo >=? 0x3a) (emitV e $ lo + 7) (emitV e lo)

-- leading 0 omitting version, 0f -> f
putHex' :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putHex' e val = do
  let hi = (val .& 0xF0) `iShiftR` 4 + (fromIntegral $ ord '0')
      lo = (val .& 0x0F) + (fromIntegral $ ord '0')

  ifte_ (hi >=? 0x3a) (emitV e $ hi + 7) (when (hi /=? (fromIntegral $ ord '0')) $ emitV e hi)
  ifte_ (lo >=? 0x3a) (emitV e $ lo + 7) (emitV e lo)

putHexArray :: (GetAlloc (AllowBreak eff) ~ 'Scope cs,
                IvoryExpr (ref s ('Stored Uint8)),
                IvoryExpr (ref s ('Array len ('Stored Uint8))), IvoryRef ref,
                GHC.TypeLits.KnownNat len) =>
               Emitter ('Stored Uint8)
               -> ref s ('Array len ('Stored Uint8)) -> Ivory eff ()
putHexArray e a = arrayMap $ \i -> do
  x <- deref (a ! i)
  putHex e x

canSend' :: AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> ChanOutput  ('Struct "can_message") -- ('Array 8 ('Stored Uint8)))
         -> Tower p ()
canSend' req msg = do
    monitor "canSender" $ do
      tx_pending <- state "tx_pending"
      last_sent  <- state "last_sent"
      handler msg "can_msg" $ do
        abort_emitter <- emitter (abortableAbort    req) 1
        req_emitter   <- emitter (abortableTransmit req) 1
        callback $ \cmsg  -> do
          refCopy last_sent cmsg

          was_pending <- deref tx_pending
          ifte_ was_pending (emitV abort_emitter true) $ do
            emit req_emitter $ constRef last_sent
            store tx_pending true

      handler (abortableComplete req) "tx_complete" $ do
        req_emitter <- emitter (abortableTransmit req) 1
        callbackV $ \ ok -> do
          ifte_ ok (store tx_pending false) $ do
            emit req_emitter $ constRef last_sent
            store tx_pending true

app :: (e -> ClockConfig)
    -> (e -> TestCAN)
    -> (e -> TestCAN)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestcan1 totestcan2 touart toleds = do
  towerDepends uartTestTypes
  towerModule  uartTestTypes

  cc <- fmap tocc getEnv
  can1  <- fmap totestcan1 getEnv
  can2  <- fmap totestcan2 getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (canctl_input, canctl_output) <- channel

  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "dma" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))
  slCANTower "slcan" ostream istream canctl_input cc can1

  (res, req, _, _) <- canTower tocc (testCAN can1) 1000000 (testCANRX can1) (testCANTX can1)
  (res2, _req2, _, _) <- canTower tocc (testCAN can2) 1000000 (testCANRX can2) (testCANTX can2)

  canSend' req canctl_output

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (testCANFilters can1)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      [CANFilterBank CANFIFO1 CANFilterMask $ CANFilter32 emptyID emptyID]
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

    received <- stateInit "can_received_count" (ival (0 :: Uint32))
    received2 <- stateInit "can2_received_count" (ival (0 :: Uint32))

    lastrecv <- state "lastrecv"

    lastid <- state "lasteid"
    lastida <- state "lasteida"

    handler res "result" $ do
      callback $ const $ do
        count <- deref received
        store received (count + 1)
        ifte_ (count .& 1 ==? 1)
          (ledOff $ redLED leds)
          (ledOn  $ redLED leds)

    handler res2 "result2" $ do
      o <- emitter ostream 64
      callback $ \msg -> do
        count <- deref received2

        refCopy lastrecv msg
        store received2 (count + 1)

        canid <- msg ~>* can_message_id
        canlen <- msg ~>* can_message_len

        isExtended <- assign $ bitToBool (canid #. can_arbitration_ide)
        isRtr <- assign $ bitToBool (canid #. can_arbitration_rtr)
        cid <- assign $ canid #. can_arbitration_id

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
                    refCopy lastida i
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

        ifte_ (count .& 1 ==? 1)
          (ledOff $ blueLED leds)
          (ledOn  $ blueLED leds)

-- http://elixir.free-electrons.com/linux/v4.11.3/source/drivers/net/can/slcan.c

-- from ASCII rep to binary
toBin :: Def ('[Uint8] ':-> Uint8)
toBin = proc "toBin" $ \x -> body $ do
 cond_
   [ x >=? (fromIntegral $ ord '0') .&& x <=? (fromIntegral $ ord '9') ==> do
       ret (x - (fromIntegral $ ord '0'))
   , x >=? (fromIntegral $ ord 'A') .&& x <=? (fromIntegral $ ord 'F') ==> do
       ret (x - (fromIntegral $ ord 'A') + 10)
   ]
 ret (x - (fromIntegral $ ord 'a') + 10)


standardIDToArray :: forall s eff . (GetAlloc eff ~ 'Scope s)
                  => Uint32
                  -> Ivory eff (ConstRef ('Stack s) ('Array 2 ('Stored Uint8)))
standardIDToArray sid = do
  slb <- assign $ sid `iShiftR` 18
  l <- local $ iarray $ fmap (ival . bitCast) [ slb `iShiftR` 8, slb]
  return $ constRef l

extendedIDToArray :: forall s eff . (GetAlloc eff ~ 'Scope s)
                  => Uint32
                  -> Ivory eff (ConstRef ('Stack s) ('Array 4 ('Stored Uint8)))
extendedIDToArray eid = do
  l <- local $ iarray $ fmap (ival . bitCast) [ eid `iShiftR` (8*i) | i <- [3,2,1,0]]
  return $ constRef l

slCANTower :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Struct "can_message")
           -> ClockConfig
           -> TestCAN
           -> Tower p ()
slCANTower greeting ostream istream canctl cc can = do
  towerDepends canDriverTypes
  towerModule  canDriverTypes

  towerDepends slcanTypes
  towerModule  slcanTypes
  p <- period (Milliseconds 1)

  monitor "echoprompt" $ do
    initialized <- stateInit "initialized" (ival false)

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

    handler p "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts o (greeting ++ "\n")

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

          store transDone false
          store transInit true

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

slcanTypes :: Module
slcanTypes = package "slcanTypes" $ do
  incl toBin
