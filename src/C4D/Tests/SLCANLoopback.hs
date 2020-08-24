{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module C4D.Tests.SLCANLoopback where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import Ivory.BSP.STM32.Peripheral.CAN.Peripheral

import Ivory.Tower.Base

import C4D.Platforms
import C4D.Types
import C4D.SLCAN

-- UART2 -> CAN1 TX -> CAN2 RX -> UART2
app :: (e -> ClockConfig)
    -> (e -> C4DPlatform)
    -> Tower e ()
app tocc toPlatform = do
  C4DPlatform{..} <- fmap toPlatform getEnv
  let redLED = platformRedLED $ basePlatform
      greenLED = platformGreenLED $ basePlatform

  c4dTowerDeps

  cc <- fmap tocc getEnv

  (canctl_input, canctl_output) <- channel
  (ostream, istream) <- bufferedUartTower tocc (platformUART basePlatform) (platformUARTPins basePlatform) 115200 (Proxy :: Proxy UARTBuffer)

  (_res,  req,  _, _) <- canTower tocc (canPeriph can1) 1000000 (canRxPin can1) (canTxPin can1)
  (res2, _req2, _, _) <- canTower tocc (canPeriph can2) 1000000 (canRxPin can2) (canTxPin can2)

  -- CAN RX + LED
  res2' <- toggleOnChanTower res2 greenLED

  slCANTower ostream istream canctl_input res2' (canReinit cc can1)

  -- CAN TX + LED
  canctl_output' <- toggleOnChanTower canctl_output redLED

  canSendTower req canctl_output'

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (canPeriphFilters can1)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      [CANFilterBank CANFIFO1 CANFilterMask $ CANFilter32 emptyID emptyID]
        ledSetup redLED
        ledSetup greenLED

  where canReinit cc can baud = canInit (canPeriph can) baud (canRxPin can) (canTxPin can) cc
