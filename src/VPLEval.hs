{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module VPLEval where

import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Effectful.Dispatch.Static
import Control.Monad
import Data.Fixed
import Data.List
import Data.Function
import qualified Data.Map.Strict as Map
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector
import VPLPretty
import VPLTypes

gameWindow :: Display
gameWindow = InWindow "VPL Turtle" (200, 200) (10, 10)

envLookup :: String -> Env -> Turtle Value
envLookup s e =
  case e Map.!? s of
    Nothing -> throwError ("Undefined variable: " ++ s)
    Just v -> return v

evalExpr :: Expr -> Env -> Turtle Float
evalExpr (Lit i) _ = return i
evalExpr (Var s) e = do
  v <- envLookup s e
  case v of
    V f -> return f
    _ -> throwError ("Cannot treat function as expression in " ++ s)
evalExpr (Add a b) e = (+) <$> evalExpr a e <*> evalExpr b e
evalExpr (Sub a b) e = (-) <$> evalExpr a e <*> evalExpr b e
evalExpr (Mul a b) e = (*) <$> evalExpr a e <*> evalExpr b e
evalExpr x@(Div a b) e = do
  a' <- evalExpr a e
  b' <- evalExpr b e
  if b' == 0
    then throwError ("Error: divide by 0 in (" ++ render (prettyExpr x) ++ ")")
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
evalStmt s@(FunCall n a) u = do
  f <- envLookup n u
  args <- mapM (`evalExpr` u) a
  case f of
    V _ ->
      throwError
        ("Cannot apply value in function call (" ++ render (prettyStmt s) ++ ")")
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

square :: Float -> Turtle ()
square n = do
  penDown
  moveForward n
  rotateTurtle 90
  moveForward n
  rotateTurtle 90
  moveForward n
  rotateTurtle 90
  moveForward n
  rotateTurtle 90
  penUp

runGame :: Turtle a -> Picture
runGame t =
  case runTurtle t mempty initState of
    Left s -> Text s
    Right (_, _, p) -> p

extEnv :: [(String, Value)] -> Env -> Env
extEnv new orig = foldl' (\m (k, v) -> Map.insert k v m) orig new

evFunDecl :: FunDecl -> Env -> TurtleFunc
evFunDecl (FunDecl name args body) u =
  \unEvArgs ->
    if length unEvArgs /= argLen
      then throwError
             (concat
                [ "Invalid number of arguments to "
                , name
                , ", expected "
                , show argLen
                , " but got "
                , show $ length unEvArgs
                , " in ("
                , render (prettyStmt (FunCall name unEvArgs))
                , ")"
                ])
      else do
        realArgs <- mapM (`evalExpr` u) unEvArgs
        mapM_ (`evalStmt` extEnv (zip args (V <$> realArgs)) u) body
  where
    argLen = length args

evProg :: [FunDecl] -> Either String Picture
evProg l =
  case finalEnv Map.!? "main" of
    Nothing -> Left "Must have a main routine"
    Just (V _) -> Left "Main must be a function"
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

showTurtle :: Either String Picture -> IO ()
showTurtle (Right p) =
  display (InWindow "VPL" (200, 200) (10, 10)) white (scale 3 3 p)
showTurtle (Left err) = putStrLn ("Error: " ++ err)

initState :: TurtleST
initState = TurtleST {pos = (0, 0), direction = -(pi / 2), pen = Up}
