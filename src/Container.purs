module Container where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Fractal (FractalParameters(..))
import FractalParametersInput as FractalParametersInput
import FractalDisplay as FractalDisplay

data Query a = UpdateParameters FractalParameters a

type State = FractalParameters

component :: forall m. H.Component HH.HTML Query Unit Void m
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
    FractalParameters
      { leftAngle: 0.1
      , rightAngle: 0.1
      , shrinkFactor: 0.8
      }

  render :: State -> H.ParentHTML Query FractalDisplay.Query Slot m
  render state =
    HH.div_
      [ HH.slot (Slot 1) FractalParametersInput.component state absurd
      , HH.slot (Slot 2) FractalDisplay.component state absurd
      ]
  
  eval :: Query ~> H.ParentDSL State Query FractalParametersInput.Query Slot Void m
  eval = case _ of
    UpdateParameters params next -> do
      H.put params
      pure next