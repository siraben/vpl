module Main where

import Prelude
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (space, spaces)
import VPLPretty
import VPLParser
import VPLEval
import VPLTypes

main = do
  l <- getArgs
  if length l /= 1
    then putStrLn "Usage ./vpl <file>"
    else parseAndShow (head l)

parseAndShow :: FilePath -> IO ()
parseAndShow s = do
  sample <- openFile s ReadMode
  s <- hGetContents sample
  let result = parse parseProg "" s
  case result of
    Right res -> mapM_ (putStrLn . renderFunDecl) res >> showTurtle (evProg res)
    Left err -> print err
  hClose sample
