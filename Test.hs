module Main where

import Indent
import Lex
import List
import ToString
import Types
import Ubi

main :: IO ()
main = do current <- getCurrentDirectory
          files   <- getFiles current
          putStrLn ("Reading " ++ show (countFiles files) ++ " files")
          let lexed    = mapDir [] lex' files
              indented = mapDir [] indent  lexed
          mapDirM writeIndent indented
          putAllIndent current
