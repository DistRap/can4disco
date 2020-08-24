
module Main where

import C4D.Platforms
import C4D.Tests.SLCANLoopback (app)

main :: IO ()
main = buildC4DApp c4d app
