module Main where

import Prelude
import Language
import Canvas (Context2D)
import CanvasInterpreter as CanvasInterpreter
import Control.Monad
import Effect (Effect)

main :: Effect Context2D
main = CanvasInterpreter.render "turtleCanvas" do
  color Purple
  star

  forward 40.0
  left 100.0

  color Red
  star

  forward 40.0
  left 100.0

  color Green
  star

star = do
  penDown
  right 144.0
  forward 100.0
  right 144.0
  forward 100.0
  right 144.0
  forward 100.0
  right 144.0
  forward 100.0
  right 144.0
  forward 100.0
  penUp
