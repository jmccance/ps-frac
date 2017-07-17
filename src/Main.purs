module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (fromJust)
import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Math (pi)
import Partial.Unsafe (unsafePartial)

import FractalParametersInput as FractalParametersInput

import Fractal (FractalParameters(..), Line(..), createTree, drawTree)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI FractalParametersInput.component unit body

main2 :: Eff (canvas :: CANVAS) Unit
main2 = do
  mcanvas <- C.getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- C.getContext2D canvas

  let trunk = Line { x: 300.0, y: 600.0, angle: (pi / 2.0), length: 100.0, width: 4.0 }
  let p = FractalParameters { leftAngle: 0.1, rightAngle: 0.1, shrinkFactor: 0.8 }
  let tree = createTree 10 p trunk
  drawTree ctx tree