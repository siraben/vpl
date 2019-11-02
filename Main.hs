module Main where

import qualified Control.Applicative as CA
import Data.Char
import System.IO
import Text.ParserCombinators.Parsec hiding (space, spaces)

import Graphics.Gloss

type Args = [Expr]

type FunName = String

type Body = [Stmt]

data FunDecl =
  FunDecl FunName Args Body
  deriving (Show)

data Expr
  = Var String
  | Lit Int
  deriving (Show)

data Stmt
  = Stop
  | TurnLeft Expr
  | TurnRight Expr
  | Forward Expr
  | PenUp
  | PenDown
  | FunCall FunName Args
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
nat :: Parser Int
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
  return (FunDecl name (Var <$> args) body)

parseFunCall :: Parser Stmt
parseFunCall = CA.liftA2 FunCall (tok ident) (sepEndBy parseExpr space)

parseExpr :: Parser Expr
parseExpr = ((Lit <$> integer) <|> (Var <$> ident)) <?> "expr"

-- do { x <- integer; return (Lit x)}
-- (integer >>= (\x -> return (Lit x)))
-- (a -> b) -> f a -> f b
-- (Int -> Expr) -> Parser Int -> Parser Expr
parseStmt :: Parser Stmt
parseStmt =
  ((symb "○" >> return PenUp) <|> (symb "●" >> return PenDown) <|>
   (symb "↑" >> (Forward <$> parseExpr)) <|>
   (symb "↻" >> (TurnRight <$> parseExpr)) <|>
   (symb "↺" >> (TurnLeft <$> parseExpr)) <|>
   parseFunCall) <?>
  "statement"

-- parseProg :: String -> Either ParseError [FunDecl]
-- parseProg s = parse (sepEndBy1 parseFunDecl space) "" s
parseProg = (space >> parseFunDecl `sepBy1` space <* eof)

main :: IO ()
main = do
  sample <- openFile "sample.txt" ReadMode
  s <- hGetContents sample
  let result = parse parseProg "" s
  case result of
    Right res -> mapM_ (putStrLn . show) res
    Left err -> print err
  hClose sample
