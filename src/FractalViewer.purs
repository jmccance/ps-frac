module FractalViewer where
  
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromJust)
import Global (readFloat, readInt)
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
type State =
  { leftAngle :: Number
  , rightAngle :: Number
  , depth :: Int
  }

data Query a =
    Render a
  | LeftAngleChanged Number a
  | RightAngleChanged Number a
  | DepthChanged Int a

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
      { leftAngle: -0.1
      , rightAngle: 0.1
      , depth: 10
      }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
    [ HH.form_
      [ HH.label
        [ HP.for "left-angle" ]
        [ HH.text "leftAngle" ]
      , HH.input
        [ HP.class_ (HH.ClassName "slider")
        , HP.name "left-angle"
        , HP.type_ HP.InputRange
        , HP.value $ show state.leftAngle
        , HP.min (-pi)
        , HP.max (pi)
        , HP.step (HP.Step 0.01)
        , HE.onValueInput $ HE.input (readFloat >>> LeftAngleChanged)
        ]
      , HH.label
        [ HP.for "right-angle" ]
        [ HH.text "rightAngle" ]
      , HH.input
        [ HP.class_ (HH.ClassName "slider")
        , HP.name "right-angle"
        , HP.type_ HP.InputRange
        , HP.value $ show state.rightAngle
        , HP.min (-pi)
        , HP.max pi
        , HP.step (HP.Step 0.01)
        , HE.onValueInput $ HE.input (readFloat >>> RightAngleChanged)
        ]
      , HH.label
        [ HP.for "depth"]
        [ HH.text "depth"]
      , HH.input
        [ HP.name "depth"
        , HP.type_ HP.InputNumber
        , HP.value $ show state.depth
        , HE.onValueInput $ HE.input (readInt 10 >>> round >>> DepthChanged)
        ]
      ]
    , HH.p_ [ HH.text $ "leftAngle = " <> (show state.leftAngle) <> ", rightAngle = " <> (show state.rightAngle) ]
    , HH.canvas [ HP.id_ "fractal-canvas", HP.width 1024 , HP.height 1024 ]
    ]
  
  eval :: Query ~> H.ComponentDSL State Query Void (Aff (canvas :: CANVAS | eff))
  eval = case _ of
    Render next -> do
      state <- H.get
      H.liftEff $ renderFractal state
      pure next

    LeftAngleChanged v next -> do
      H.modify (_ { leftAngle = v })
      state <- H.get
      H.liftEff $ renderFractal state
      pure next

    RightAngleChanged v next -> do
      H.modify (_ { rightAngle = v })
      state <- H.get
      H.liftEff $ renderFractal state
      pure next

    DepthChanged v next -> do
      H.modify (_ { depth = v })
      state <- H.get
      H.liftEff $ renderFractal state
      pure next

renderFractal :: forall eff. State -> Eff (canvas :: CANVAS | eff) Unit
renderFractal state = do
  mcanvas <- C.getCanvasElementById "fractal-canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  dims <- C.getCanvasDimensions canvas
  ctx <- C.getContext2D canvas

  _ <- C.clearRect ctx { x: 0.0, y: 0.0, w: dims.width, h: dims.height }

  let params = FractalParameters { leftAngle: state.leftAngle, rightAngle: state.rightAngle, shrinkFactor: 0.8 }
  let trunk = Line { x: 512.0, y: 512.0, angle: (pi / 2.0), length: 100.0, width: 4.0 }
  let tree = createTree state.depth params trunk
  drawTree ctx tree