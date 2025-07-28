-- | Main entry point for the VPL interpreter
--   Handles command-line arguments, file parsing, and program execution.
module Main where

import Prelude
import Control.Exception (catch, IOException)
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (space, spaces)
import VPLPretty
import VPLParser
import VPLEval
import VPLTypes

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> parseAndShow filepath
    _ -> putStrLn "Usage: vpl <file>"

parseAndShow :: FilePath -> IO ()
parseAndShow s = do
  sample <- openFile s ReadMode
  s <- hGetContents sample
  let result = parse parseProg "" s
  case result of
    Right res -> mapM_ (putStrLn . renderFunDecl) res >> showTurtle (evProg res)
    Left err -> print err
  hClose sample
