module FractalView where
  
import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (fromJust)
import Graphics.Canvas as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (pi)
import Partial.Unsafe (unsafePartial)

import Fractal (FractalParameters(..), Line(..), createTree, drawTree)

type Input = 
  { leftAngle :: Number
  , rightAngle :: Number
  , shrinkFactor :: Number
  , depth :: Int
  }

type State = Unit

data Query a = HandleInput Input a

type Message = Void

component :: forall eff.
  H.Component HH.HTML Query Input Message (Aff (canvas :: C.CANVAS | eff))
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: HE.input HandleInput
    }

  where
  render :: State -> H.ComponentHTML Query
  render s =
    HH.canvas [ HP.id_ "fractal-canvas", HP.width 1024, HP.height 768 ]
  
  eval ::
    Query ~> H.ComponentDSL State Query Message (Aff (canvas :: C.CANVAS | eff))
  eval = case _ of
    HandleInput input next -> do
      H.liftEff $ do
        mcanvas     <-  C.getCanvasElementById "fractal-canvas"
        let canvas  =   unsafePartial (fromJust mcanvas)
        dims        <-  C.getCanvasDimensions canvas
        ctx         <-  C.getContext2D canvas
        _           <-  C.clearRect
                          ctx
                          { x: 0.0, y: 0.0, w: dims.width, h: dims.height }
        
        let tree    = createTree input.depth (paramsFromState input) trunk
        drawTree ctx tree
      pure next

    where
      paramsFromState state = 
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
  

