module Main where

import Prelude

import Control.Monad.Eff (Eff)
import PsFrac as PsFrac
import Graphics.Canvas (CANVAS)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body  <- HA.awaitBody
  runUI PsFrac.component unit body
