
module Main where

import C4D.Platforms
import C4D.Tests.SLCAN (app)

main :: IO ()
main = buildC4DApp c4d app
