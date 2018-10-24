module Main (main) where

import Indent
import Lex
import List
import Prelude hiding (lex)
import System.Directory(getCurrentDirectory)
import ToString
import Type.DirFile(countFiles, mapDir, mapDirM)

main :: IO ()
main = do current <- getCurrentDirectory
          files   <- getFiles current
          putStrLn ("Reading " ++ show (countFiles files) ++ " files")
          let lexed    = mapDir [] lex    files
              indented = mapDir [] indent lexed
          mapDirM writeIndent indented
          putAllIndent current
