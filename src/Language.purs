module Language where

import Prelude
import Control.Monad
import Control.Monad.Free (Free, liftF)


type Angle = Number
type Distance = Number
data Color = Red | Green | Blue | Yellow | Purple | Magenta | Cyan | Black | White | CustomColor String

data TurtleCmd a = Forward Distance a
                 | Arc Distance Angle a
                 | Right Angle a
                 | PenUp a
                 | PenDown a
                 | UseColor Color a

instance turtleCmd :: Functor TurtleCmd where
  map f (Forward dist r)     = Forward dist (f r)
  map f (Arc radius angle r) = Arc radius angle (f r)
  map f (Right angle r)      = Right angle (f r)
  map f (PenUp r)            = PenUp (f r)
  map f (PenDown r)          = PenDown (f r)
  map f (UseColor col r)     = UseColor col (f r)

instance turtleCmdShow :: (Show a) => Show (TurtleCmd a) where
  show x = "(TurtleCmd)"

type TurtleProg = Free TurtleCmd

forward :: Distance -> TurtleProg Unit
forward n = liftF (Forward n unit)

arc :: Distance -> Angle -> TurtleProg Unit
arc radius angle = liftF (Arc radius angle unit)

right :: Angle -> TurtleProg Unit
right angle = liftF (Right angle unit)

left :: Angle -> TurtleProg Unit
left angle = right (360.0 - angle)

penUp :: TurtleProg Unit
penUp = liftF (PenUp unit)

penDown :: TurtleProg Unit
penDown = liftF (PenDown unit)

color :: Color -> TurtleProg Unit
color col = liftF (UseColor col unit)
