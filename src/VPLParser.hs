module VPLParser where

import qualified Control.Applicative as CA
import Data.Char
import System.IO
import Text.ParserCombinators.Parsec hiding (space, spaces)
import VPLEval
import VPLTypes

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
initp x = isAlpha x || (not (isSpace x) && notElem x "○●[↑↻↺,=]-")

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
  FunDecl name args <$> parseBody

parseFunCall :: Parser Stmt
parseFunCall = CA.liftA2 FunCall (tok ident) (many parseExpr)

parseExpr :: Parser Expr
parseExpr = tok ((Lit <$> integer) <|> (Var <$> ident)) <?> "expr"

parseLoop :: Parser Stmt
parseLoop = do
  symb "loop"
  x <- integer
  Loop (round x) <$> parseBody

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
   parseLoop <||> parseFunCall) <?>
  "statement"

-- parseProg :: String -> Either ParseError [FunDecl]
parseProg = space >> parseFunDecl `sepBy1` space <* eof

parseAndShow :: FilePath -> IO ()
parseAndShow s = do
  sample <- openFile s ReadMode
  s <- hGetContents sample
  let result = parse parseProg "" s
  case result of
    Right res -> mapM_ print res >> showTurtle (evProg res)
    Left err -> print err
  hClose sample
