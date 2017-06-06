{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module C4D.Types where

import Ivory.Language
import Ivory.Tower

[ivory| string struct UARTBuffer 1024 |]

c4dTypes :: Module
c4dTypes = package "c4dTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)


c4dTowerDeps :: Tower e ()
c4dTowerDeps = do
  towerDepends c4dTypes
  towerModule c4dTypes
