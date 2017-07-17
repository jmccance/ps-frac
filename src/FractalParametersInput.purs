module FractalParametersInput where
  
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math as Math

type Input = Unit
type State = Unit

data Query a = HandleInput Unit a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_ 
    [
      HH.form_
        [ HH.label_ [ HH.text "leftAngle" ]
        , HH.input
          [ HP.type_ HP.InputRange
          , HP.value "0.1"
          , HP.min (-2.0 * Math.pi)
          , HP.max (2.0 * Math.pi)
          , HP.step (HP.Step 0.01)
          ]
        , HH.label_ [ HH.text "rightAngle" ]
        , HH.input
          [ HP.type_ HP.InputRange
          , HP.value "0.1"
          , HP.min (-2.0 * Math.pi)
          , HP.max (2.0 * Math.pi)
          , HP.step (HP.Step 0.01)
          ]
      ],
      HH.canvas
        [ HP.width 800
        , HP.height 800
        ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput n next -> do
      pure next