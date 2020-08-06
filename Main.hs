module Main (main) where

import Data.List (partition)
import Import
import Lex (lex')
import Parse (parse)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix ((</>), takeExtension)

main :: IO ()
main = do contents <- listRecursive "."

          files <- mapM readFile contents

          let lexed  = concatMap lex' $ zip contents files
              parsed = parse lexed

          return ()

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
