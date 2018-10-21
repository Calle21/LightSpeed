module Nova.Test where

import Nova.Indent
import Nova.Lex
import Nova.List
import Nova.ToString
import Nova.Ubi
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do currentDir <- getCurrentDirectory
          files <- getFiles currentDir
          let lexed    = mapDir lex    files
              indented = mapDir indent lexed
          mapDirM putIndent indented
