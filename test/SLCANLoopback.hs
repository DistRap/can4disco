
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import C4D.Platforms
import C4D.Tests.SLCANLoopback (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_can1
            testplatform_can2
            testplatform_uart
            testplatform_leds
  where
  p topts = getConfig topts testPlatformParser
