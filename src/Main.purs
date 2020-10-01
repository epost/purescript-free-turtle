module Main where

import Prelude
import Language
import Canvas (Context2D)
import CanvasInterpreter
import Control.Monad
import Effect (Effect)

main :: Effect Context2D
main = renderTurtleProgOnCanvas "turtleCanvas" $ do
  star
  penUp
  forward 40.0
  left 100.0
  penDown
  color Red
  star

star = do
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
