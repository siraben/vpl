{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Core types and effects for the VPL (Visual Programming Language)
--   This module defines the abstract syntax tree, turtle state,
--   error types, and effect handling for the VPL interpreter.
module VPLTypes where

import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local

import qualified Data.Map.Strict as Map
import Graphics.Gloss

-- | Function argument names
type Args = [String]

-- | Function name
type FunName = String

-- | Function body (sequence of statements)
type Body = [Stmt]

-- | Function declaration with name, arguments, and body
data FunDecl =
  FunDecl FunName Args Body
  deriving (Show)

-- | Arithmetic expressions
data Expr
  = Var String      -- ^ Variable reference
  | Lit Float       -- ^ Numeric literal
  | Add Expr Expr   -- ^ Addition
  | Mul Expr Expr   -- ^ Multiplication
  | Sub Expr Expr   -- ^ Subtraction
  | Div Expr Expr   -- ^ Division
  deriving (Eq, Show)

-- | Boolean expressions
data BExpr =
  IsZero Expr       -- ^ Test if expression equals zero
  deriving (Show)

data Stmt
  = TurnLeft Expr
  | TurnRight Expr
  | Forward Expr
  | PenUp
  | PenDown
  | FunCall FunName [Expr]
  | Loop Expr Body
  | If BExpr Body Body
  deriving (Show)

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

type TurtleFunc = [Expr] -> Turtle ()

data Value
  = Function TurtleFunc
  | V Float

type Env = Map.Map String Value

-- | A function callable in the VPL language
type VPLFunction = [Expr] -> Turtle ()

data VPLError
  = UndefinedVariable String
  | NotAFunction String
  | NotAValue String
  | DivisionByZero String
  | InvalidArgumentCount String Int Int
  | NoMainFunction
  | MainNotFunction
  | ParseError String
  | FileError String
  deriving (Show, Eq)

-- | The effects needed for turtle graphics
type TurtleEffects = '[Error VPLError, State TurtleST, Writer Picture, Reader Env]

-- | The turtle monad
type Turtle = Eff TurtleEffects

-- | Constraint for functions that work with any effect stack containing turtle effects
type TurtleConstraints es = 
  ( Error VPLError :> es
  , State TurtleST :> es
  , Writer Picture :> es
  , Reader Env :> es
  )

-- | Run the turtle computation with all effects handled
runTurtle :: Turtle a -> Env -> TurtleST -> Either VPLError (a, TurtleST, Picture)
runTurtle action env st = 
  runPureEff $ do
    result <- runReader env 
            . runWriter 
            . runState st 
            . runErrorNoCallStack 
            $ action
    case result of
      (((Left e, _), _)) -> return (Left e)
      (((Right a, s), w)) -> return (Right (a, s, w))

-- | Format an error for display
formatError :: VPLError -> String
formatError (UndefinedVariable var) = "Undefined variable: " ++ var
formatError (NotAFunction name) = "'" ++ name ++ "' is not a function"
formatError (NotAValue name) = "'" ++ name ++ "' is not a value"
formatError (DivisionByZero expr) = "Division by zero in: " ++ expr
formatError (InvalidArgumentCount name expected got) = 
  "Function '" ++ name ++ "' expects " ++ show expected ++ 
  " arguments but got " ++ show got
formatError NoMainFunction = "No main function defined"
formatError MainNotFunction = "main must be a function"
formatError (ParseError msg) = "Parse error: " ++ msg
formatError (FileError msg) = "File error: " ++ msg
