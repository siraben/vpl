module Main where

import System.Environment
import VPLParser

main = do
  l <- getArgs
  if length l /= 1
    then putStrLn "Usage ./vpl <file>"
    else parseAndShow (head l)
