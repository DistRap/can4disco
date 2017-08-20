{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.OS.Posix.Tower
import Ivory.OS.Posix.Tower.Serial

import C4D.SLCAN
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types

app :: Tower e ()
app = do
  uartTowerDeps

  (buffered_ostream, istream) <- serialIO
  ostream <- uartUnbuffer (
    buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  -- SLCAN loopback
  (toCanIn, toCanOut) <- channel
  slCANTower ostream istream toCanIn toCanOut (const (return ()))

main :: IO ()
main = compileTowerPosix (const $ return ()) app
