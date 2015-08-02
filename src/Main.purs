module Main where

import Prelude
import Language
import CanvasInterpreter
import Canvas (Context2DEff (..))
import Util
import Control.Monad
import Control.Monad.Eff


main :: Context2DEff
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
