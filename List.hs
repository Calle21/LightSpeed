module Nova.List (getFiles)

import System.Directory
import Control.Monad(filterM)
import System.FilePath.Posix(takeExtension)

getFiles :: FilePath -> IO Directory
getFiles dir = do contents    <- listDirectory currentDir
                  novafiles   <- filterM novaPred contents
                  maps        <- filterM doesDirectoryExist contents
                  abscontents <- mapM makeAbsolute contents
                  subfiles    <- mapM getFiles abscontents
                  return (subfiles ++ novafiles)
  where
  novaPred :: String -> IO Bool
  novaPred s = do f <- doesFileExist s
                  return (f && takeExtension s == ".nova")
