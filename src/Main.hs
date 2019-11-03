module Main where
import VPLParser
import System.Environment



main = do
  l <- getArgs
  if length l /= 1
    then putStrLn "Usage ./vpl <file>"
    else parseAndShow (head l)
