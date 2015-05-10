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


-- | x, y, rotation, isPenDown
data Turtle = Turtle Distance Distance Angle Boolean

instance turtleShow :: Show Turtle where
  show (Turtle x y angle isPenDown) = "(Turtle " ++ show x ++ " " ++ show y ++ " " ++ show angle ++ " " ++ show isPenDown ++ ")"


compileTurtleProg :: forall a. TurtleProg a -> Context2D -> Context2DEff
compileTurtleProg turtleProg ctx = foldl (>>=) (pure ctx) (compileTurtleProg' turtleProg)


compileTurtleProg' :: forall a. TurtleProg a -> [Context2D -> Context2DEff]
compileTurtleProg' turtleProg =

  evalState turtleProgState (Turtle 0 0 0 true)

  where turtleProg' = const [] <$> turtleProg
        turtleProgState = compileTurtleProg'' turtleProg'


-- | A natural transformation from `TurtleProg` to `State Turtle`.
compileTurtleProg'' :: TurtleProg   [Context2D -> Context2DEff]
                    -> State Turtle [Context2D -> Context2DEff]
compileTurtleProg'' = runFreeM compileCmd

  -- pick off the outermost TurtleCmd from the TurtleProg and process it
  where compileCmd :: TurtleCmd    (TurtleProg [Context2D -> Context2DEff])
                   -> State Turtle (TurtleProg [Context2D -> Context2DEff]) 
    
        compileCmd (Forward r rest) = do
          Turtle x y angle p <- get
          let x' = x + adjacent r angle
              y' = y + opposite r angle
              instr = lineTo x' y'
          put (Turtle x' y' angle p)

          return ((\prog -> prog ++ [instr]) <$> rest)

        compileCmd (Arc r arcAngleDeg rest) = do
          Turtle x y turtleAngle p <- get
          let angleEnd = turtleAngle + rad arcAngleDeg
              angle'   = angleEnd + rad 90
              x'       = x + adjacent r angleEnd
              y'       = y + opposite r angleEnd
              instr    = drawArc x y r turtleAngle angleEnd

          put (Turtle x' y' angle' p)
          pure (rest <#> (++ [instr]))

        compileCmd (Right angleDeg rest) = do
          let angle = rad angleDeg
          modify $ \(Turtle x y angle0 p) -> Turtle x y (angle0 + angle) p
          return rest

        compileCmd (PenUp rest) = do
          modify $ \(Turtle x y angle _) -> Turtle x y angle false
          return ((\prog -> prog ++ [endStroke]) <$> rest)

        compileCmd (PenDown rest) = do
          Turtle x y angle p <- get
          put (Turtle x y angle true)
          return ((\prog -> prog ++ [beginStroke, moveTo x y]) <$> rest)

        compileCmd (UseColor col rest) = do
          return ((\prog -> prog ++ [setStrokeStyle $ colorToCanvasStyle col]) <$> rest)


adjacent r angle = r * cos angle
opposite r angle = r * sin angle
rad angleDegrees = (2 * pi * (angleDegrees % 360)) / 360

colorToCanvasStyle :: Color -> String
colorToCanvasStyle col = case col of
  Red -> "red"
  Green -> "green"
  Blue -> "blue"
  Purple -> "purple"
  Black -> "black"
  CustomColor str -> str

renderTurtleProgOnCanvas :: String -> TurtleProg Unit -> Context2DEff
renderTurtleProgOnCanvas canvasId prog =
  get2DContext canvasId >>=
  initContext (colorToCanvasStyle Purple) >>=
  moveTo 0 0 >>=
  beginStroke >>=
  compileTurtleProg prog >>=
  endStroke
