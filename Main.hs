{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Control.Applicative as CA
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State
import Data.Char
import Data.Fixed
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (State(..), space, spaces)

type Args = [String]

type FunName = String

type Body = [Stmt]

data FunDecl =
  FunDecl FunName Args Body
  deriving (Show)

data Expr
  = Var String
  | Lit Float
  deriving (Show)

data Stmt
  = TurnLeft Expr
  | TurnRight Expr
  | Forward Expr
  | PenUp
  | PenDown
  | FunCall FunName [Expr]
  deriving (Show)

a <||> b = try a <|> b

-- |Transforms a parser into one that also consumes trailing whitespace.
tok p = p <* space

-- parens p = lparen >> p <* rparen
eol = char '\n'

comment = symb "--" >> many (noneOf "\n") >> eol

whitespace = oneOf " \n\t" <|> comment <?> "whitespace"

space :: Parser ()
space = skipMany whitespace

-- |Parse a symbol, consuming leading whitespace.
symb :: String -> Parser String
symb = tok . string

-- |Parse a number.
nat :: Parser Float
nat = read <$> many1 digit

-- |Like 'nat' but consumes leading whitespace.
natural = tok nat

-- |Parse a negative number.
negnat = string "-" >> (-) 0 <$> natural

integer = negnat <|> natural

-- initp x = x `elem` concat [['a' .. 'z'], ['A' .. 'Z']] -- , "!$%&*/:<>?_~"]
initp x = isAlpha x || (not (isSpace x) && not (x `elem` "○●[↑↻↺,=]-"))

digitp x = x `elem` ['0' .. '9']

specialSubseqp x = x `elem` "+-.@"

subseqp c = initp c || digitp c || specialSubseqp c

ident =
  do x <- satisfy initp
     xs <- many (satisfy subseqp)
     return (x : xs)
     <|> string "+" <|>
  string "-" <|>
  try (string "...") <?> "identifier"

-- data FunDecl = FunDecl FunName Args Body
--              deriving Show
parseBody :: Parser Body
parseBody = do
  symb "["
  x <- sepBy1 parseStmt (symb ",")
  symb "]"
  return x

parseFunDecl :: Parser FunDecl
parseFunDecl = do
  (name:args) <- sepEndBy1 ident space
  symb "="
  body <- parseBody
  return (FunDecl name args body)

parseFunCall :: Parser Stmt
parseFunCall = CA.liftA2 FunCall (tok ident) (sepEndBy parseExpr space)

parseExpr :: Parser Expr
parseExpr = ((Lit <$> integer) <|> (Var <$> ident)) <?> "expr"

-- do { x <- integer; return (Lit x)}
-- (integer >>= (\x -> return (Lit x)))
-- (a -> b) -> f a -> f b
-- (Float -> Expr) -> Parser Float -> Parser Expr
parseStmt :: Parser Stmt
parseStmt =
  ((symb "○" >> return PenUp) <|> (symb "●" >> return PenDown) <|>
   (symb "↑" >> (Forward <$> parseExpr)) <|>
   (symb "↻" >> (TurnRight <$> parseExpr)) <|>
   (symb "↺" >> (TurnLeft <$> parseExpr)) <|>
   parseFunCall) <?>
  "statement"

-- parseProg :: String -> Either ParseError [FunDecl]
parseProg = (space >> parseFunDecl `sepBy1` space <* eof)

parseAndShow :: FilePath -> IO ()
parseAndShow s = do
  sample <- openFile s ReadMode
  s <- hGetContents sample
  let result = parse parseProg "" s
  case result of
    Right res -> mapM_ print res >> showTurtle (evProg res)
    Left err -> print err
  hClose sample

data PenStatus
  = Up
  | Down
  deriving (Show)

data TurtleST =
  TurtleST
    { direction :: Float
    , pos :: Point
    , pen :: PenStatus
    }
  deriving (Show)

gameWindow :: Display
gameWindow = InWindow "VPL Turtle" (200, 200) (10, 10)

type TurtleFunc = [Expr] -> Turtle ()

data Value
  = Function TurtleFunc
  | V Float

type Env = Map.Map String Value

newtype Turtle a =
  Turtle
    { runTurtle :: ExceptT String (RWS Env Picture TurtleST) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState TurtleST
           , MonadWriter Picture
           , MonadReader Env
           , MonadError [Char]
           )

envLookup :: String -> Env -> Turtle Value
envLookup s e = do
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
  args <- mapM (\e -> evalExpr e u) a
  case f of
    V _ -> throwError "Cannot apply value"
    Function f -> f (Lit <$> args)

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
  (\unEvArgs ->
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
         realArgs <- mapM (\x -> evalExpr x u) unEvArgs
         mapM_ (\s -> evalStmt s (extEnv (zip args (V <$> realArgs)) u)) body)
  where
    argLen = length args

evProg :: [FunDecl] -> Either String Picture
evProg l =
  case finalEnv Map.!? "main" of
    Nothing -> Left "Must have a main routine"
    Just (Function t) ->
      Right $ third $ runRWS (runExceptT (runTurtle (t []))) finalEnv initState
  where
    finalEnv =
      (foldl'
         (\r f@(FunDecl n _ _) -> Map.insert n (Function (evFunDecl f r)) r)
         mempty
         l)

showTurtle :: Either String Picture -> IO ()
showTurtle (Right p) = display (InWindow "Turtle" (200, 200) (10, 10)) white p
showTurtle (Left err) =
  display (InWindow "Turtle" (200, 200) (10, 10)) white (text err)

main = do
  l <- getArgs
  if length l /= 1
    then putStrLn "Usage ./vpl <file>"
    else parseAndShow (l !! 0)

initState :: TurtleST
initState = TurtleST {pos = (0, 0), direction = -(pi / 2), pen = Up}
