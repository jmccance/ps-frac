module FractalViewer where
  
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromJust)
import Fractal (FractalParameters(..), Line(..), createTree, drawTree)
import Global (readInt)
import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB
import Math (pi)
import Partial.Unsafe (unsafePartial)

import Slider as Slider

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

data Message = ParametersUpdated

data Slot =
    LeftAngleSlot
  | RightAngleSlot
  | ShrinkFactorSlot
derive instance eqSliderSlot :: Eq Slot
derive instance ordSliderSlot :: Ord Slot

component :: forall eff.
  H.Component HH.HTML Query Input Message (Aff (canvas :: CANVAS | eff))
component =
  H.parentComponent
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

    render ::
      State
      -> H.ParentHTML Query Slider.Query Slot (Aff (canvas :: CANVAS | eff))
    render state =
      HH.div
      [ HP.class_ HB.container ]
      [ HH.form_
        [ HH.h1_ [ HH.text "ps-frac" ]
        , HH.div
          [ HP.class_ HB.formGroup ]
          [ HH.label
            [ HP.for "left-angle"
            , HP.class_ HB.controlLabel ]
            [ HH.text $ "left branch angle = " <> (show state.leftAngle) ]
          , HH.slot
              LeftAngleSlot
              (Slider.slider
                { name: "left-angle"
                , min: -2.0
                , max: 2.0
                , step: (HP.Step 0.01)
                , value: state.leftAngle
                })
              unit
              (HE.input LeftAngleChanged)
          ]
        , HH.div
          [ HP.class_ HB.formGroup ]
          [ HH.label
            [ HP.for "right-angle"
            , HP.class_ HB.controlLabel ]
            [ HH.text $ "right branch angle = " <> (show state.rightAngle) ]
          , HH.slot
              RightAngleSlot
              (Slider.slider
                { name: "right-angle"
                , min: -2.0
                , max: 2.0
                , step: (HP.Step 0.01)
                , value: state.rightAngle
                })
              unit
              (HE.input RightAngleChanged)
          ]
        , HH.div
          [ HP.class_ HB.formGroup ]
          [ HH.label
            [ HP.for "shrink-factor" ]
            [ HH.text $ "shrink factor = " <> (show state.shrinkFactor) ]
          , HH.slot
              ShrinkFactorSlot
              (Slider.slider
                { name: "shrink-factor"
                , min: 0.0
                , max: 1.0
                , step: (HP.Step 0.01)
                , value: state.shrinkFactor
                })
              unit
              (HE.input ShrinkFactorChanged)
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
    
    eval ::
      Query ~> H.ParentDSL State Query Slider.Query Slot Message (Aff (canvas :: CANVAS | eff))
    eval = case _ of
      Render next -> do
        state <- H.get
        H.liftEff $ drawFractal state
        pure next

      DepthChanged v next -> do
        H.modify (_ { depth = v })
        H.raise ParametersUpdated
        pure next

      LeftAngleChanged v next -> do
        H.modify (_ { leftAngle = v })
        H.raise ParametersUpdated
        pure next

      RightAngleChanged v next -> do
        H.modify (_ { rightAngle = v })
        H.raise ParametersUpdated
        pure next

      ShrinkFactorChanged v next -> do
        H.modify (_ { shrinkFactor = v })
        H.raise ParametersUpdated
        pure next

drawFractal :: forall eff. State -> Eff (canvas :: CANVAS | eff) Unit
drawFractal state =
  do
    mcanvas     <-  C.getCanvasElementById "fractal-canvas"
    let canvas  =   unsafePartial (fromJust mcanvas)
    dims        <-  C.getCanvasDimensions canvas
    ctx         <-  C.getContext2D canvas
    _           <-  C.clearRect
                      ctx
                      { x: 0.0, y: 0.0, w: dims.width, h: dims.height }
    
    let tree    = createTree state.depth params trunk

    drawTree ctx tree
  where
    params = 
      FractalParameters
      { leftAngle: state.leftAngle
      , rightAngle: state.rightAngle
      , shrinkFactor: state.shrinkFactor
      }

    trunk =
      Line 
        { x: 512.0
        , y: 512.0
        , angle: (pi / 2.0)
        , length: 100.0
        , width: 4.0 }