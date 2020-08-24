{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module C4D.Platforms
  (
    buildC4DApp
  , C4DPlatform(..)
  , c4d
  , module Hello.Tests.Platforms
  )
  where

import qualified Ivory.BSP.STM32F407 as F407
import Ivory.Tower.Base

import Ivory.Tower
import Ivory.BSP.STM32.ClockConfig

import Hello.Tests.Platforms.F4DISCO
import Hello.Tests.Platforms

data C4DPlatform = C4DPlatform {
    basePlatform :: Platform
  , can1         :: CANConfig
  , can2         :: CANConfig
  , can1TxLED    :: LED
  , can1RxLED    :: LED
  , can2TxLED    :: LED
  , can2RxLED    :: LED
  }

-- wrapped f4disco helloworld Platform
c4d :: C4DPlatform
c4d = C4DPlatform {
    basePlatform = f4disco
  , can1         = f4discoCAN1
  , can2         = f4discoCAN2
  , can1TxLED    = LED F407.pinC8 ActiveHigh
  , can1RxLED    = LED F407.pinC6 ActiveHigh
  , can2TxLED    = LED F407.pinE8 ActiveHigh
  , can2RxLED    = LED F407.pinE10 ActiveHigh
  }


buildC4DApp :: C4DPlatform
            -> (
                    (C4DPlatform -> ClockConfig)
                 -> (C4DPlatform -> C4DPlatform)
                 -> Tower C4DPlatform ()
               )
            -> IO ()
buildC4DApp = buildWrappedApp unWrap wrap
 where
   unWrap :: C4DPlatform -> Platform
   unWrap = basePlatform

   wrap :: Platform -> C4DPlatform
   wrap x = c4d { basePlatform = x }

