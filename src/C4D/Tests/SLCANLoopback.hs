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

import Ivory.Tower.Base

import C4D.Platforms
import C4D.Types
import C4D.SLCAN

-- UART2 -> CAN1 TX -> CAN2 RX -> UART2
app :: (e -> ClockConfig)
    -> (e -> TestCAN)
    -> (e -> TestCAN)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestcan1 totestcan2 touart toleds = do
  c4dTowerDeps

  cc <- fmap tocc getEnv
  can1  <- fmap totestcan1 getEnv
  can2  <- fmap totestcan2 getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (canctl_input, canctl_output) <- channel
  (ostream, istream) <- bufferedUartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200 (Proxy :: Proxy UARTBuffer)

  (_res, req, _, _) <- canTower tocc (testCAN can1) 1000000 (testCANRX can1) (testCANTX can1)
  (res2, _req2, _, _) <- canTower tocc (testCAN can2) 1000000 (testCANRX can2) (testCANTX can2)

  -- CAN RX + LED
  res2' <- toggleOnChanTower res2 (blueLED leds)

  slCANTower ostream istream canctl_input res2' (canReinit cc can1)

  -- CAN TX + LED
  canctl_output' <- toggleOnChanTower canctl_output (redLED leds)

  canSendTower req canctl_output'

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (testCANFilters can1)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      [CANFilterBank CANFIFO1 CANFilterMask $ CANFilter32 emptyID emptyID]
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

  where canReinit cc can baud = canInit (testCAN can) baud (testCANRX can) (testCANTX can) cc
