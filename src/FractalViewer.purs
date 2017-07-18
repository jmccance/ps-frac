module FractalViewer where
  
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), fromJust)
import Global (readFloat)
import Graphics.Canvas as C
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (pi)
import Partial.Unsafe (unsafePartial)

import Fractal (FractalParameters(..), Line(..), createTree, drawTree)

type Input = Unit
type State = FractalParameters

data Query a =
    LeftAngleChanged Number a
  | RightAngleChanged Number a

component :: forall eff. H.Component HH.HTML Query Input Void (Aff (canvas :: CANVAS | eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    FractalParameters
      { leftAngle: 0.0
      , rightAngle: 0.0
      , shrinkFactor: 0.8
      }

  render :: State -> H.ComponentHTML Query
  render (FractalParameters state) =
    HH.div_
    [ HH.form_
      [ HH.label
        [ HP.for "left-angle" ]
        [ HH.text $ "leftAngle" ]
      , HH.input
        [ HP.class_ (HH.ClassName "slider")
        , HP.type_ HP.InputRange
        , HP.value $ show state.leftAngle
        , HP.min (-pi)
        , HP.max (pi)
        , HP.step (HP.Step 0.01)
        , HE.onValueInput $ HE.input (readFloat >>> LeftAngleChanged)
        ]
      , HH.label
        [ HP.for "right-angle" ]
        [ HH.text $ "rightAngle" ]
      , HH.input
        [ HP.class_ (HH.ClassName "slider")
        , HP.type_ HP.InputRange
        , HP.value $ show state.rightAngle
        , HP.min (-pi)
        , HP.max pi
        , HP.step (HP.Step 0.01)
        , HE.onValueInput $ HE.input (readFloat >>> RightAngleChanged)
        ]
      ]
    , HH.canvas [ HP.id_ "fractal-canvas", HP.width 1024 , HP.height 1024 ]
    ]
  
  eval :: Query ~> H.ComponentDSL State Query Void (Aff (canvas :: CANVAS | eff))
  eval = case _ of
    LeftAngleChanged v next -> do
      FractalParameters p <- H.get
      H.put (FractalParameters $ _ { leftAngle = v } p)
      params <- H.get
      H.liftEff $ renderFractal params
      pure next
    RightAngleChanged v next -> do
      FractalParameters p <- H.get
      H.put (FractalParameters $ _ { rightAngle = v } p)
      params <- H.get
      H.liftEff $ renderFractal params
      pure next


renderFractal :: forall eff. FractalParameters -> Eff (canvas :: CANVAS | eff) Unit
renderFractal params = do
  mcanvas <- C.getCanvasElementById "fractal-canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  dims <- C.getCanvasDimensions canvas
  ctx <- C.getContext2D canvas

  _ <- C.clearRect ctx { x: 0.0, y: 0.0, w: dims.width, h: dims.height }

  let trunk = Line { x: 512.0, y: 512.0, angle: (pi / 2.0), length: 100.0, width: 4.0 }
  let tree = createTree 10 params trunk
  drawTree ctx tree