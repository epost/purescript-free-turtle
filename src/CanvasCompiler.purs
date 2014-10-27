module CanvasCompiler where

import Canvas
import Language
import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.Eff
import Data.Tuple
import Data.Foldable
import Math (sin, cos, pi)


-- | x, y, rotation
data Turtle = Turtle Number Number Angle

instance turtleShow :: Show Turtle where
  show (Turtle x y angle) = "(Turtle " ++ show x ++ " " ++ show y ++ " " ++ show angle ++ ")"


compileTurtleProg :: forall a. TurtleProg a -> Context2D -> Context2DEff
compileTurtleProg turtleProg ctx = foldl (>>=) (pure ctx) (compileTurtleProg' turtleProg)


compileTurtleProg' :: forall a. TurtleProg a -> [Context2D -> Context2DEff]
compileTurtleProg' turtleProg =

  evalState turtleProgState (Turtle 0 0 0)

  where turtleProg' = const [] <$> turtleProg
        turtleProgState = compileTurtleProg'' turtleProg'


compileTurtleProg'' :: TurtleProg   [Context2D -> Context2DEff]
                    -> State Turtle [Context2D -> Context2DEff]
compileTurtleProg'' = goM compileCmd

  -- pick off the outermost TurtleCmd from the TurtleProg and process it
  where compileCmd :: TurtleCmd    (TurtleProg [Context2D -> Context2DEff])
                   -> State Turtle (TurtleProg [Context2D -> Context2DEff]) 
    
        compileCmd (Forward r rest) = do

          Turtle x y angle <- get
          
          let x' = x + adjacent r angle
              y' = y + opposite r angle
              instr = lineTo x' y'
          
          put (Turtle x' y' angle)

          return ((\prog -> prog ++ [instr]) <$> rest)

        
        compileCmd (Right angleDeg rest) = do

          let angle = rad angleDeg

          modify $ \(Turtle x y angle0) -> Turtle x y (angle0 + angle)

          return rest
        

adjacent r angle = r * cos angle
opposite r angle = r * sin angle
rad angleDegrees = (2 * pi * (angleDegrees % 360)) / 360
