{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VPLTypes where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State

import qualified Data.Map.Strict as Map
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector

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
  | Loop Int Body
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
           , MonadError String
           )
