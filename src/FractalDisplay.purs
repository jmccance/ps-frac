module FractalDisplay where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Fractal (FractalParameters(..))

type Input = Unit
type State = FractalParameters

data Query a = ParametersUpdated FractalParameters a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component {
    initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where

  initialState :: State
  initialState = FractalParameters
    { leftAngle: 0.0, rightAngle: 0.0, shrinkFactor: 0.0 }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.canvas
        [ HP.width 800
        , HP.height 800
        ]
      ]
  
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ParametersUpdated params next -> do
      H.put params
      pure next