module Main (main) where

import Data.List (partition)
import GetTypes
import Import
import Infix
import Lex (lex')
import GetBindings (getBindings)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix ((</>), takeExtension, takeFileName)

main :: IO ()
main = do contents <- listRecursive "."

          usefile <- readFile "use"
          let use = imp usefile

          infixfile <- readFile' "infix"
          let inf = readInfixFile infixfile

          files <- mapM readFile contents
          let lexed  = concat $ map lex' (zip
                                           (map takeFileName contents)
                                           files)
              types  = getTypes lexed
              protos = getProtos lexed'

listRecursive :: FilePath -> IO [String]
listRecursive path = do dirExists <- doesDirectoryExist path
                        if dirExists
                        then do contents <- listDirectory' path
                                let (files,other) = partition (\p -> takeExtension p == ".nova")
                                subfiles <- mapM listRecursive other
                                return files ++ (concat subfiles)
                        else return []

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path = map (path </>) `fmap` listDirectory path

readFile' :: FilePath -> IO String
readFile' path = do exists <- doesFileExist path
                    if exists
                    then readFile path
                    else return []
