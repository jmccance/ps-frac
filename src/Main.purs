module Main where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import FractalViewer as FractalViewer
import FractalViewer (Query(..))

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body  <- HA.awaitBody
  io    <- runUI FractalViewer.component unit body
 
  -- Listen for changes to the parameters and redraw the fractal when they
  -- change.
  io.subscribe $ CR.consumer \(FractalViewer.ParametersUpdated) -> do
    io.query $ H.action $ Render
    pure Nothing

  -- Send an initial Render message to kick the system into gear.
  io.query $ H.action $ Render