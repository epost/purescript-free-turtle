module BrowserMain where

import Language
import CanvasCompiler
import Canvas
import Util
import Control.Monad
import Control.Monad.Eff
import Debug.Trace


main :: Context2DEff
main =
  get2DContext "turtleCanvas" >>=
  beginStroke >>=
  compileTurtleProg star >>=
  endStroke

star = do
  right 144
  forward 100
  right 144
  forward 100
  right 144
  forward 100
  right 144
  forward 100
  right 144
  forward 100

star' = forward 100 >> star
