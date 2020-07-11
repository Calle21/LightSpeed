module Main (main) where

import Infix

main :: IO ()
main = do infile <- readFile "infix"
          let inf = readInfixFile infile
          print inf
