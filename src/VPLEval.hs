module VPLEval where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State
import Data.Fixed
import Data.List
import qualified Data.Map.Strict as Map
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector
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
    _ -> throwError "Cannot treat function as expression"

penUp = modify (\s -> s {pen = Up})

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
    V _ -> throwError "Cannot apply value"
    Function f -> f (Lit <$> args)
evalStmt (Loop n b) u = replicateM_ n (mapM_ (`evalStmt` u) b)

-- evalStmt (TurnLeft e) = e 
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

third (_, _, x) = x

runGame :: Turtle a -> Picture
runGame t =
  case runRWS (runExceptT (runTurtle t)) mempty initState of
    (Left s, _, _) -> Text s
    (_, _, p) -> p

extEnv :: [(String, Value)] -> Env -> Env
extEnv new orig = foldl' (\m (k, v) -> Map.insert k v m) orig new

evFunDecl :: FunDecl -> Env -> TurtleFunc
evFunDecl (FunDecl name args body) u =
  \unEvArgs ->
    if length unEvArgs /= argLen
      then throwError
             (concat
                [ "Invalid number of arguments to: "
                , name
                , ", expected "
                , show argLen
                , " but got "
                , show $ length unEvArgs
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
    Just (Function t) ->
      case runRWS (runExceptT (runTurtle (t []))) finalEnv initState of
        (Left e, _, _) -> Left e
        (_, _, p) -> Right p
  where
    finalEnv =
      foldl'
        (\r f@(FunDecl n _ _) -> Map.insert n (Function (evFunDecl f r)) r)
        mempty
        l

showTurtle :: Either String Picture -> IO ()
showTurtle (Right p) = display (InWindow "VPL" (200, 200) (10, 10)) white (scale 3 3 p)
showTurtle (Left err) = do
  putStrLn ("Error: " ++ err)

initState :: TurtleST
initState = TurtleST {pos = (0, 0), direction = -(pi / 2), pen = Up}
