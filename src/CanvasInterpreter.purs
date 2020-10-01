module CanvasInterpreter where

import Prelude
import Canvas
import Effect
import Language
import Control.Monad
import Control.Monad.Free (runFreeM)
import Control.Monad.State (State, evalState, get, modify_, put)
import Data.Tuple
import Data.Foldable
import Math (sin, cos, pi, (%))

-- | x, y, rotation, isPenDown
data Turtle = Turtle Distance Distance Angle Boolean

instance turtleShow :: Show Turtle where
  show (Turtle x y angle isPenDown) = "(Turtle " <> show x <> " " <> show y <> " " <> show angle <> " " <> show isPenDown <> ")"


interpretTurtleProg :: forall a. TurtleProg a -> Context2D -> Effect Context2D
interpretTurtleProg turtleProg ctx = foldl (>>=) (pure ctx) (interpretTurtleProg' turtleProg)


interpretTurtleProg' :: forall a. TurtleProg a -> Array (Context2D -> Effect Context2D)
interpretTurtleProg' turtleProg =

  evalState turtleProgState (Turtle 0.0 0.0 0.0 true)

  where turtleProg' = const [] <$> turtleProg
        turtleProgState = interpretTurtleProg'' turtleProg'


-- | A natural transformation from `TurtleProg` to `State Turtle`.
interpretTurtleProg'' :: TurtleProg   (Array (Context2D -> Effect Context2D))
                      -> State Turtle (Array (Context2D -> Effect Context2D))
interpretTurtleProg'' = runFreeM interpret

  -- pick off the outermost TurtleCmd from the TurtleProg and process it
  where interpret :: TurtleCmd    (TurtleProg (Array (Context2D -> Effect Context2D)))
                  -> State Turtle (TurtleProg (Array (Context2D -> Effect Context2D)))
    
        interpret (Forward r rest) = do
          Turtle x y angle p <- get
          let x' = x + adjacent r angle
              y' = y + opposite r angle
              instr = lineTo x' y'
          put (Turtle x' y' angle p)

          pure ((\prog -> prog <> [instr]) <$> rest)

        interpret (Arc r arcAngleDeg rest) = do
          Turtle x y turtleAngle p <- get
          let angleEnd = turtleAngle + rad arcAngleDeg
              angle'   = angleEnd + rad 90.0
              x'       = x + adjacent r angleEnd
              y'       = y + opposite r angleEnd
              instr    = drawArc x y r turtleAngle angleEnd

          put (Turtle x' y' angle' p)
          pure (rest <#> (_ <> [instr]))

        interpret (Right angleDeg rest) = do
          let angle = rad angleDeg
          modify_ \(Turtle x y angle0 p) -> Turtle x y (angle0 + angle) p
          pure rest

        interpret (PenUp rest) = do
          modify_ $ \(Turtle x y angle _) -> Turtle x y angle false
          pure ((\prog -> prog <> [stroke]) <$> rest)

        interpret (PenDown rest) = do
          Turtle x y angle p <- get
          put (Turtle x y angle true)
          pure ((\prog -> prog <> [beginPath, moveTo x y]) <$> rest)

        interpret (UseColor col rest) = do
          pure ((\prog -> prog <> [setStrokeStyle $ colorToCanvasStyle col]) <$> rest)


adjacent r angle = r * cos angle
opposite r angle = r * sin angle
rad angleDegrees = (2.0 * pi * (angleDegrees % 360.0)) / 360.0

renderTurtleProgOnCanvas :: String -> TurtleProg Unit -> Effect Context2D
renderTurtleProgOnCanvas canvasId prog =
  get2DContext canvasId >>=
  initContext (colorToCanvasStyle Purple) >>=
  moveTo 0.0 0.0 >>=
  interpretTurtleProg prog
