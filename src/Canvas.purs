module Canvas where

import Prelude
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Language (Distance (), Angle (), Color (..))

foreign import data Context2D :: Type

type CanvasStyleString = String

foreign import get2DContext :: String -> Effect Context2D

foreign import initContext :: CanvasStyleString -> Context2D -> Effect Context2D

foreign import beginPath :: Context2D -> Effect Context2D

foreign import closePath :: Context2D -> Effect Context2D

foreign import stroke :: Context2D -> Effect Context2D

foreign import lineTo :: Distance -> Distance -> Context2D -> Effect Context2D

drawArc :: Distance -> Distance -> Distance -> Angle -> Angle -> Context2D -> Effect Context2D
drawArc = drawFilledArc' "transparent"

drawFilledArc :: Maybe Color -> Distance -> Distance -> Distance -> Angle -> Angle -> Context2D -> Effect Context2D
drawFilledArc col = drawFilledArc' $ maybe "" colorToCanvasStyle col

foreign import drawFilledArc' :: CanvasStyleString -> Distance -> Distance -> Distance -> Angle -> Angle -> Context2D -> Effect Context2D

foreign import moveTo :: Distance -> Distance -> Context2D -> Effect Context2D

foreign import setStrokeStyle :: String -> Context2D -> Effect Context2D

colorToCanvasStyle :: Color -> String
colorToCanvasStyle col = case col of
  Red -> "red"
  Green -> "green"
  Blue -> "blue"
  Yellow -> "yellow"
  Purple -> "purple"
  Cyan -> "cyan"
  Magenta -> "magenta"
  Black -> "black"
  White -> "white"
  CustomColor str -> str
