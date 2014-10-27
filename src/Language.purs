module Language where

import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.State.Class


type Angle = Number

data TurtleCmd a = Forward Number a
                 | Right Angle a

instance turtleCmd :: Functor TurtleCmd where
  (<$>) f (Forward dist r) = Forward dist (f r) 
  (<$>) f (Right angle r)  = Right angle (f r) 

instance turtleCmdShow :: (Show a) => Show (TurtleCmd a) where
  show x = "(TurtleCmd)"

type TurtleProg = Free TurtleCmd

forward :: Number -> forall a. (TurtleProg Unit)
forward n = liftF (Forward n unit)

right :: Angle -> forall a. (TurtleProg Unit)
right angle = liftF (Right angle unit)

left angle = right (360 - angle)
