{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

-- | Evaluator for the VPL (Visual Programming Language)
--   This module implements the turtle graphics interpreter,
--   including statement evaluation and picture generation.
module VPLEval where

import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Control.Monad
import Data.Fixed
import Data.List
import qualified Data.Map.Strict as Map
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector
import VPLPretty
import VPLTypes

-- Configuration constants
windowTitle :: String
windowTitle = "VPL"

windowSize :: (Int, Int)
windowSize = (200, 200)

windowPosition :: (Int, Int)
windowPosition = (10, 10)

scaleFactor :: Float
scaleFactor = 3.0

initialDirection :: Float
initialDirection = -(pi / 2)

initialPosition :: Point
initialPosition = (0, 0)


envLookup :: String -> Env -> Turtle Value
envLookup s e =
  case e Map.!? s of
    Nothing -> throwError (UndefinedVariable s)
    Just v -> return v

evalExpr :: Expr -> Env -> Turtle Float
evalExpr (Lit i) _ = return i
evalExpr (Var s) e = do
  v <- envLookup s e
  case v of
    V f -> return f
    _ -> throwError (NotAValue s)
evalExpr (Add a b) e = (+) <$> evalExpr a e <*> evalExpr b e
evalExpr (Sub a b) e = (-) <$> evalExpr a e <*> evalExpr b e
evalExpr (Mul a b) e = (*) <$> evalExpr a e <*> evalExpr b e
evalExpr x@(Div a b) e = do
  a' <- evalExpr a e
  b' <- evalExpr b e
  if b' == 0
    then throwError (DivisionByZero (render (prettyExpr x)))
    else return (a' / b')

evalBExpr :: BExpr -> Env -> Turtle Bool
evalBExpr (IsZero x) e = (== (0 :: Integer)) . floor <$> evalExpr x e

penUp :: Turtle ()
penUp = modify (\s -> s {pen = Up})

penDown :: Turtle ()
penDown = modify (\s -> s {pen = Down})

evalStmt :: Stmt -> Env -> Turtle ()
evalStmt PenUp _ = penUp
evalStmt PenDown _ = penDown
evalStmt (Forward e) u = evalExpr e u >>= moveForward
evalStmt (TurnLeft e) u = evalExpr e u >>= rotateTurtle
evalStmt (TurnRight e) u = evalExpr e u >>= rotateTurtle <$> negate
evalStmt (FunCall n a) u = do
  f <- envLookup n u
  args <- mapM (`evalExpr` u) a
  case f of
    V _ -> throwError (NotAFunction n)
    Function f' -> f' (Lit <$> args)
evalStmt (Loop n b) u = do
  x <- evalExpr n u
  replicateM_ (floor x) (mapM_ (`evalStmt` u) b)
evalStmt (If p c a) u = do
  b <- evalBExpr p u
  mapM_ (`evalStmt` u) (if b then c else a)

lineFrom :: Point -> Turtle ()
lineFrom p = do
  curr <- gets pos
  tell (Line [curr, p])

-- |Move the turtle in the given vector, returning the new position.
moveBy :: Point -> Turtle ()
moveBy v = do
  oldPos <- gets pos
  modify (\s -> s {pos = oldPos P.+ v})

-- |Rotate the turtle by the number of given degrees
rotateTurtle :: Float -> Turtle ()
rotateTurtle a = do
  d <- gets direction
  modify (\s -> s {direction = mod' (d + degToRad a) (2 * pi)})

degToRad :: Floating a => a -> a
degToRad x = (x / 360) * (2 * pi)

-- |Move forward.  If the pen is down, a line is drawn.
moveForward :: Float -> Turtle ()
moveForward f = do
  p <- gets pen
  old <- gets pos
  d <- gets direction
  moveBy (rotateV d (0, f))
  case p of
    Up -> return ()
    Down -> lineFrom old



extEnv :: [(String, Value)] -> Env -> Env
extEnv new orig = foldl' (\m (k, v) -> Map.insert k v m) orig new

evFunDecl :: FunDecl -> Env -> TurtleFunc
evFunDecl (FunDecl name args body) u =
  \unEvArgs ->
    if length unEvArgs /= argLen
      then throwError (InvalidArgumentCount name argLen (length unEvArgs))
      else do
        realArgs <- mapM (`evalExpr` u) unEvArgs
        mapM_ (`evalStmt` extEnv (zip args (V <$> realArgs)) u) body
  where
    argLen = length args

evProg :: [FunDecl] -> Either VPLError Picture
evProg l =
  case finalEnv Map.!? "main" of
    Nothing -> Left NoMainFunction
    Just (V _) -> Left MainNotFunction
    Just (Function t) ->
      case runTurtle (t []) finalEnv initState of
        Left e -> Left e
        Right (_, _, p) -> Right p
  where
    finalEnv =
      foldl'
        (\r f@(FunDecl n _ _) -> Map.insert n (Function (evFunDecl f r)) r)
        mempty
        l

showTurtle :: Either VPLError Picture -> IO ()
showTurtle (Right p) =
  display (InWindow windowTitle windowSize windowPosition) white (scale scaleFactor scaleFactor p)
showTurtle (Left err) = putStrLn ("Error: " ++ formatError err)

initState :: TurtleST
initState = TurtleST {pos = initialPosition, direction = initialDirection, pen = Up}
