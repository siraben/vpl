module VPLParser where

import qualified Control.Applicative as CA
import Data.Char
import Text.ParserCombinators.Parsec hiding (space, spaces)
import VPLTypes

a <||> b = try a <|> b

-- |Transforms a parser into one that also consumes trailing whitespace.
tok p = p <* space

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
negnat = char '-' >> (-) 0 <$> natural

integer = negnat <|> natural

initp x = isAlpha x || (not (isSpace x) && notElem x "()○●[↑↻↺,=]-")

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

identifier = tok ident

parseBody :: Parser Body
parseBody = do
  symb "["
  x <- sepBy parseStmt (symb ",")
  symb "]"
  return x

parseFunDecl :: Parser FunDecl
parseFunDecl = do
  (name:args) <- many1 identifier
  symb "="
  FunDecl name args <$> parseBody

parseFunCall :: Parser Stmt
parseFunCall = CA.liftA2 FunCall (tok ident) (many parseExpr)

parseFactor :: Parser Expr
parseFactor =
  (Lit <$> integer) <|> (Var <$> identifier) <|>
  (symb "(" >> parseExpr <* symb ")")

parseTerm :: Parser Expr
parseTerm =
  (do a <- parseFactor
      op <- (symb "*" >> return Mul) <|> (symb "/" >> return Div)
      op a <$> parseFactor) <||>
  parseFactor

parseExpr :: Parser Expr
parseExpr =
  ((do a <- parseTerm
       symb "+"
       Add a <$> parseTerm) <||>
   parseTerm) <?>
  "expression"

parseLoop :: Parser Stmt
parseLoop = do
  symb "loop"
  x <- parseExpr
  Loop x <$> parseBody

parseBExpr :: Parser BExpr
parseBExpr = (symb "zero?" >> (IsZero <$> parseExpr)) <?> "boolean expression"

parseIf :: Parser Stmt
parseIf =
  (do symb "if"
      p <- parseBExpr
      symb "then"
      c <- parseBody
      symb "else"
      a <- parseBody
      return (If p c a)) <?>
  "if statement"

parseStmt :: Parser Stmt
parseStmt =
  ((symb "○" >> return PenUp) <|>
   (symb "●" >> return PenDown) <|>
   (symb "↑" >> (Forward <$> parseExpr)) <|>
   (symb "↻" >> (TurnRight <$> parseExpr)) <|>
   (symb "↺" >> (TurnLeft <$> parseExpr)) <|>
   parseIf <||> parseLoop <||> parseFunCall) <?>
  "statement"

parseProg = space >> parseFunDecl `sepBy` space <* eof
