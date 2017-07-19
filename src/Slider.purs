module Slider where

import Prelude

import Data.Maybe (Maybe(..))
import Global (readFloat)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

type Input = Unit

data Query a = SetValue Number a

type State =
  { name :: String
  , min :: Number
  , max :: Number
  , step :: HP.StepValue
  , value :: Number }

type Message = Number

slider :: forall m. State -> H.Component HH.HTML Query Input Message m
slider initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where
  render :: State -> H.ComponentHTML Query
  render s = 
    HH.input
      [ HP.class_ HB.formControl
      , HP.name s.name
      , HP.type_ HP.InputRange
      , HP.min s.min
      , HP.max s.max
      , HP.step s.step
      , HP.value $ show s.value
      , HE.onValueInput $ HE.input (readFloat >>> SetValue)
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    SetValue newValue next -> do
      H.modify (_ { value = newValue })
      H.raise $ newValue
      pure next