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
  initContext >>=
  beginStroke >>=
  moveTo 0 0 >>=
  compileTurtleProg stars >>=
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

stars = do
  star
  penUp
  forward 100
  left 100
  penDown
  star
