{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module C4D.Tests.CANLoop where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter

import Ivory.Tower.Base

import C4D.Platforms

canSend :: AbortableTransmit ('Struct "can_message") ('Stored IBool)
        -> Tower e ()
canSend req = do
    periodic <- period (Milliseconds 500)

    monitor "canSender" $ do
      tx_pending <- state "tx_pending"
      last_sent  <- state "last_sent"
      handler periodic "periodic" $ do
        abort_emitter <- emitter (abortableAbort    req) 1
        req_emitter   <- emitter (abortableTransmit req) 1
        callbackV $ \ p -> do
          let time  :: Uint64
              time  = signCast $ toIMicroseconds p
          let msgid = standardCANID (fromRep 0x7FF) (boolToBit false)
          r <- local $ istruct
            [ can_message_id  .= ival msgid
            , can_message_buf .= iarray
                [ ival $ bitCast $ time `iShiftR` fromInteger (8 * i)
                | i <- [7,6..0]
                ]
            , can_message_len .= ival 8
            ]
          refCopy last_sent r

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
    -> (e -> C4DPlatform)
    -> Tower e ()
app tocc toPlatform = do
  C4DPlatform{..} <- fmap toPlatform getEnv
  let redLED = platformRedLED $ basePlatform
      greenLED = platformGreenLED $ basePlatform

  (res, req, _, _) <- canTower tocc (canPeriph can1)   1000000 (canRxPin can1) (canTxPin can1)
  (res2, req2, _, _) <- canTower tocc (canPeriph can2) 1000000 (canRxPin can2) (canTxPin can2)

  canSend req
  canSend req2

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (canPeriphFilters can1)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      [CANFilterBank CANFIFO1 CANFilterMask $ CANFilter32 emptyID emptyID]
        ledSetup redLED
        ledSetup greenLED

    received <- stateInit "can_received_count" (ival (0 :: Uint32))
    received2 <- stateInit "can2_received_count" (ival (0 :: Uint32))

    handler res "result" $ do
      callback $ const $ do
        count <- deref received
        store received (count + 1)
        ifte_ (count .& 1 ==? 0)
          (ledOff redLED)
          (ledOn  redLED)

    handler res2 "result2" $ do
      callback $ const $ do
        count <- deref received2
        store received2 (count + 1)
        ifte_ (count .& 1 ==? 0)
          (ledOff greenLED)
          (ledOn  greenLED)
