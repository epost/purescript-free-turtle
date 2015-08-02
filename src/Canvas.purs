module Canvas where

import Prelude
import Data.Maybe
import Control.Monad.Eff
import Language (Distance (), Angle (), Color (..))

foreign import data Context2D :: *

foreign import data DOM :: !

type Context2DEff = Eff (dom :: DOM) Context2D

type CanvasStyleString = String

foreign import get2DContext :: forall eff. String -> Context2DEff

foreign import initContext :: forall eff. CanvasStyleString -> Context2D -> Context2DEff

foreign import beginStroke :: forall eff. Context2D -> Context2DEff

foreign import endStroke :: forall eff. Context2D -> Context2DEff

foreign import lineTo :: Distance -> Distance -> Context2D -> Context2DEff

drawArc :: Distance -> Distance -> Distance -> Angle -> Angle -> Context2D -> Context2DEff
drawArc = drawFilledArc' "transparent"

drawFilledArc :: Maybe Color -> Distance -> Distance -> Distance -> Angle -> Angle -> Context2D -> Context2DEff
drawFilledArc col = drawFilledArc' $ maybe "" colorToCanvasStyle col

foreign import drawFilledArc' :: CanvasStyleString -> Distance -> Distance -> Distance -> Angle -> Angle -> Context2D -> Context2DEff

foreign import moveTo :: Distance -> Distance -> Context2D -> Context2DEff

foreign import setStrokeStyle :: String -> Context2D -> Context2DEff

colorToCanvasStyle :: Color -> String
colorToCanvasStyle col = case col of
  Red -> "red"
  Green -> "green"
  Blue -> "blue"
  Purple -> "purple"
  Black -> "black"
  CustomColor str -> str
