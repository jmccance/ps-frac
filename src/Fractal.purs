module Fractal where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Tuple (Tuple(..))
import Graphics.Canvas as C
import Graphics.Canvas (CANVAS, Context2D)
import Math (cos, pi, sin)

-- Fractal code adapted from 
-- http://blog.ploeh.dk/2017/06/06/fractal-trees-with-purescript

data Tree a = Leaf a | Node a (Tree a) (Tree a)

data Line = Line
  { x :: Number
  , y :: Number
  , angle :: Number
  , length :: Number
  , width :: Number
  }

endpoint :: forall r.
  {
    x :: Number
  , y :: Number
  , angle :: Number
  , length :: Number
  | r }
  -> Tuple Number Number
endpoint line =
  Tuple
    (line.x + line.length * cos line.angle)
    (-(-line.y + line.length * sin line.angle))

data FractalParameters = FractalParameters
  { leftAngle :: Number
  , rightAngle :: Number
  , shrinkFactor :: Number
  }

createBranches :: FractalParameters -> Line -> Tuple Line Line
createBranches (FractalParameters p) (Line line) =
  Tuple left right
  where
    Tuple x y = endpoint line
    left = Line
      { x: x
      , y: y
      , angle: pi * (line.angle / pi - p.leftAngle)
      , length: (line.length * p.shrinkFactor)
      , width: (line.width * p.shrinkFactor)
      }
    right = Line
      { x: x
      , y: y
      , angle: pi * (line.angle / pi - p.rightAngle)
      , length: (line.length * p.shrinkFactor)
      , width: (line.width * p.shrinkFactor)
      }

createTree :: Int -> FractalParameters -> Line -> Tree Line
createTree depth p line =
  if depth <= 0
  then Leaf line
  else
    let Tuple leftLine rightLine = createBranches p line
        left = createTree (depth - 1) p leftLine
        right = createTree (depth - 1) p rightLine
    in Node line left right

drawLine :: forall eff. Context2D -> Line -> Eff (canvas :: CANVAS | eff) Unit
drawLine ctx (Line line) = do
  let Tuple x' y' = endpoint line
  void $ C.strokePath ctx $ do
    void $ C.moveTo ctx line.x line.y
    void $ C.setLineWidth line.width ctx
    void $ C.lineTo ctx x' y'
    C.closePath ctx

drawTree :: forall eff. Context2D -> Tree Line -> Eff (canvas :: CANVAS | eff) Unit
drawTree ctx (Leaf line) = drawLine ctx line
drawTree ctx (Node line left right) = do
  drawLine ctx line
  drawTree ctx left
  drawTree ctx right
