module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import FractalViewer as FractalViewer
import FractalViewer (Query(..))

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI FractalViewer.component unit body
  io.query $ H.action $ LeftAngleChanged 0.0 
