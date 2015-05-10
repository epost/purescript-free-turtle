module Main where

import Language
import CanvasCompiler
import Canvas
import Util
import Control.Monad
import Control.Monad.Eff
import Debug.Trace


main :: Context2DEff
main = renderTurtleProgOnCanvas "turtleCanvas" $ do
  star
  penUp
  forward 40
  left 100
  penDown
  color Red
  star

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
