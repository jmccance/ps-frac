module FractalViewer where
  
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromJust)
import Fractal (FractalParameters(..), Line(..), createTree, drawTree)
import Global (readFloat, readInt)
import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
import Halogen as H
import Halogen.HTML (a, div)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB
import Math (pi)
import Partial.Unsafe (unsafePartial)

type Input = Unit
type State =
  { leftAngle :: Number
  , rightAngle :: Number
  , shrinkFactor :: Number
  , depth :: Int
  }

data Query a =
    Render a
  | DepthChanged Int a
  | LeftAngleChanged Number a
  | RightAngleChanged Number a
  | ShrinkFactorChanged Number a

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
      , shrinkFactor: 0.8
      , depth: 10
      }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
    [ HP.class_ HB.container ]
    [ HH.form_
      [ HH.div
        [ HP.class_ HB.formGroup ]
        [ HH.label
          [ HP.for "left-angle" ]
          [ HH.text $ "left branch angle = " <> (show state.leftAngle) ]
        , HH.input
          [ HP.classes [ HB.formControl, (HH.ClassName "slider") ]
          , HP.name "left-angle"
          , HP.type_ HP.InputRange
          , HP.value $ show state.leftAngle
          , HP.min (-2.0)
          , HP.max 2.0
          , HP.step (HP.Step 0.01)
          , HE.onValueInput $ HE.input (readFloat >>> LeftAngleChanged)
          ]
        ]
      , HH.div
        [ HP.class_ HB.formGroup ]
        [ HH.label
          [ HP.for "right-angle" ]
          [ HH.text $ "right branch angle = " <> (show state.rightAngle) ]
        , HH.input
          [ HP.classes [ HB.formControl, (HH.ClassName "slider") ]
          , HP.name "right-angle"
          , HP.type_ HP.InputRange
          , HP.value $ show state.rightAngle
          , HP.min (-2.0)
          , HP.max 2.0
          , HP.step (HP.Step 0.01)
          , HE.onValueInput $ HE.input (readFloat >>> RightAngleChanged)
          ]
        ]
      , HH.div
        [ HP.class_ HB.formGroup ]
        [ HH.label
          [ HP.for "shrink-factor" ]
          [ HH.text $ "shrink factor = " <> (show state.shrinkFactor) ]
        , HH.input
          [ HP.classes [ HB.formControl, (HH.ClassName "slider") ]
          , HP.name "shrink-factor"
          , HP.type_ HP.InputRange
          , HP.value $ show state.shrinkFactor
          , HP.min 0.0
          , HP.max 1.0
          , HP.step (HP.Step 0.01)
          , HE.onValueInput $ HE.input (readFloat >>> ShrinkFactorChanged)
          ]
        ]
      , HH.div
        [ HP.class_ HB.formGroup ]
        [ HH.label
          [ HP.for "depth"]
          [ HH.text "depth"]
        , HH.input
          [ HP.name "depth"
          , HP.class_ HB.formControl
          , HP.type_ HP.InputNumber
          , HP.value $ show state.depth
          , HE.onValueInput $ HE.input (readInt 10 >>> round >>> DepthChanged)
          ]
        ]
      ]
    , HH.canvas [ HP.id_ "fractal-canvas", HP.width 1024 , HP.height 1024 ]
    ]
  
  eval :: Query ~> H.ComponentDSL State Query Void (Aff (canvas :: CANVAS | eff))
  eval = case _ of
    Render next -> do
      state <- H.get
      H.liftEff $ renderFractal state
      pure next

    DepthChanged v next -> do
      H.modify (_ { depth = v })
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

    ShrinkFactorChanged v next -> do
      H.modify (_ { shrinkFactor = v })
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
  let params = FractalParameters { leftAngle: state.leftAngle, rightAngle: state.rightAngle, shrinkFactor: state.shrinkFactor }
  let trunk = Line { x: 512.0, y: 512.0, angle: (pi / 2.0), length: 100.0, width: 4.0 }
  let tree = createTree state.depth params trunk
  drawTree ctx tree