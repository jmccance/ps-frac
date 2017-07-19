module PsFrac where
  
import Prelude

import Control.Monad.Aff (Aff)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import FractalView as FractalView
import Global (readInt)
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB
import Slider as Slider

type Input = Unit
type State =
  { leftAngle :: Number
  , rightAngle :: Number
  , shrinkFactor :: Number
  , depth :: Int
  }

data Query a =
    Initialize a
  | DepthChanged Int a
  | LeftAngleChanged Number a
  | RightAngleChanged Number a
  | ShrinkFactorChanged Number a

data Message = ParametersUpdated

data SliderSlot =
    LeftAngleSlot
  | RightAngleSlot
  | ShrinkFactorSlot

derive instance eqSliderSlot :: Eq SliderSlot
derive instance ordSliderSlot :: Ord SliderSlot

data ViewSlot = FractalViewSlot

derive instance eqViewSlot :: Eq ViewSlot
derive instance ordViewSlot :: Ord ViewSlot

type Slot = Either2 SliderSlot ViewSlot

type ChildQuery = Coproduct2 Slider.Query FractalView.Query

sliderSlot :: CP.ChildPath Slider.Query ChildQuery SliderSlot Slot
sliderSlot = CP.cp1

viewSlot :: CP.ChildPath FractalView.Query ChildQuery ViewSlot Slot
viewSlot = CP.cp2

component :: forall eff.
  H.Component HH.HTML Query Input Message (Aff (canvas :: CANVAS | eff))
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
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
      -> H.ParentHTML Query ChildQuery Slot (Aff (canvas :: CANVAS | eff))
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
          , HH.slot'
              sliderSlot LeftAngleSlot
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
          , HH.slot'
              sliderSlot RightAngleSlot
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
          , HH.slot'
              sliderSlot ShrinkFactorSlot
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
      , HH.slot'
          viewSlot FractalViewSlot
          FractalView.component
          state
          absurd
      ]
    
    eval ::
      Query 
      ~> H.ParentDSL 
         State
         Query
         ChildQuery
         Slot
         Message
         (Aff (canvas :: CANVAS | eff))
    eval = case _ of
      Initialize next -> do
        -- Hacky nonsense to force the children to use their input on start.
        -- get/put alone won't do it, but apparently modify will even if we
        -- don't actually change anything.
        s <- H.get
        H.modify $ _ { shrinkFactor = s.shrinkFactor }
        pure next

      DepthChanged v next -> do
        H.modify (_ { depth = v })
        pure next

      LeftAngleChanged v next -> do
        H.modify (_ { leftAngle = v })
        pure next

      RightAngleChanged v next -> do
        H.modify (_ { rightAngle = v })
        pure next

      ShrinkFactorChanged v next -> do
        H.modify (_ { shrinkFactor = v })
        pure next
