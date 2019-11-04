module Main where

import Prelude
import System.Environment
import VPLParser
import VPLTypes

main = do
  l <- getArgs
  if length l /= 1
    then putStrLn "Usage ./vpl <file>"
    else parseAndShow (head l)
