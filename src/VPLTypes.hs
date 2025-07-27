{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module VPLTypes where

import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local

import qualified Data.Map.Strict as Map
import Graphics.Gloss

type Args = [String]

type FunName = String

type Body = [Stmt]

data FunDecl =
  FunDecl FunName Args Body
  deriving (Show)

data Expr
  = Var String
  | Lit Float
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

data BExpr =
  IsZero Expr
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

type Turtle = Eff '[Error String, State TurtleST, Writer Picture, Reader Env]

runTurtle :: Turtle a -> Env -> TurtleST -> (Either String (a, TurtleST, Picture))
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
