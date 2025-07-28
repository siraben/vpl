-- | Parser for the VPL (Visual Programming Language)
--   This module provides parsers for VPL syntax including expressions,
--   statements, function declarations, and complete programs.
module VPLParser where

import Data.Char
import Text.ParserCombinators.Parsec hiding (space, spaces)
import VPLTypes

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

-- | Transforms a parser into one that also consumes trailing whitespace.
tok :: Parser a -> Parser a
tok p = p <* space

-- | Parse end of line
eol :: Parser Char
eol = char '\n'

-- | Parse a comment starting with "--"
comment :: Parser Char  
comment = symb "--" *> many (noneOf "\n") *> eol

-- | Parse whitespace including comments
whitespace :: Parser Char
whitespace = oneOf " \n\t" <|> comment <?> "whitespace"

-- | Skip zero or more whitespace characters
space :: Parser ()
space = skipMany whitespace

-- | Parse a symbol, consuming leading whitespace.
symb :: String -> Parser String
symb = tok . string

-- | Parse a natural number.
nat :: Parser Float
nat = read <$> many1 digit

-- | Like 'nat' but consumes leading whitespace.
natural :: Parser Float
natural = tok nat

-- | Parse a negative number.
negNat :: Parser Float
negNat = char '-' *> (negate <$> natural)

-- | Parse an integer (positive or negative)
integer :: Parser Float
integer = negNat <|> natural

-- | Test if character can start an identifier
initp :: Char -> Bool
initp x = isAlpha x || (not (isSpace x) && notElem x "()○●[↑↻↺,=]-")

-- | Test if character is a digit
digitp :: Char -> Bool
digitp x = x `elem` ['0' .. '9']

-- | Test if character can appear in special subsequent positions
specialSubseqp :: Char -> Bool
specialSubseqp x = x `elem` "+-.@"

-- | Test if character can appear after first character in identifier
subseqp :: Char -> Bool
subseqp c = initp c || digitp c || specialSubseqp c

-- | Parse an identifier
ident :: Parser String
ident =
  (((:) <$> satisfy initp <*> many (satisfy subseqp)) <|> string "+" <|>
   string "-") <?>
  "identifier"

-- | Parse an identifier with trailing whitespace consumed
identifier :: Parser String
identifier = tok ident

parseBody :: Parser Body
parseBody = symb "[" *> sepBy parseStmt (symb ",") <* symb "]" <?> "statement body"

parseFunDecl :: Parser FunDecl
parseFunDecl = do
  (name:args) <- many1 identifier <* symb "="
  FunDecl name args <$> parseBody <?> "function declaration"

parseFunCall :: Parser Stmt
parseFunCall = FunCall <$> tok ident <*> many parseExpr <?> "function call"

parseFactor :: Parser Expr
parseFactor =
  ((Lit <$> integer) <|> (Var <$> identifier) <|>
   (symb "(" *> parseExpr <* symb ")")) <?> "number, variable, or parenthesized expression"

parseTerm :: Parser Expr
parseTerm =
  (((symb "*" *> return Mul) <|> (symb "/" *> return Div)) <*> parseFactor <*>
   parseFactor) <||>
  parseFactor

parseExpr :: Parser Expr
parseExpr =
  ((Add <$> parseTerm <* symb "+" <*> parseTerm) <||> parseTerm) <?>
  "expression"

parseLoop :: Parser Stmt
parseLoop = Loop <$> (symb "loop" *> parseExpr) <*> parseBody

parseBExpr :: Parser BExpr
parseBExpr = (symb "zero?" *> (IsZero <$> parseExpr)) <?> "boolean expression"

parseIf :: Parser Stmt
parseIf =
  (If <$> (symb "if" *> parseBExpr) <*> (symb "then" *> parseBody) <*>
   (symb "else" *> parseBody)) <?>
  "if statement"

parseStmt :: Parser Stmt
parseStmt =
  ((symb "○" *> return PenUp) <|> (symb "●" *> return PenDown) <|>
   (symb "↑" *> (Forward <$> parseExpr)) <|>
   (symb "↻" *> (TurnRight <$> parseExpr)) <|>
   (symb "↺" *> (TurnLeft <$> parseExpr)) <|>
   parseIf <||> parseLoop <||> parseFunCall) <?>
  "statement"

parseProg :: Parser [FunDecl]
parseProg = space *> parseFunDecl `sepBy` space <* eof
