module List where

import qualified Data.ByteString.Char8 as C
import Syntax
import Types
import Ubi
import Util

filterNewFiles :: [FilePath] -> [UTCTime] -> [UTCTime] -> [FilePath]
filterNewFiles oldf oldst hidst
  | null oldf               = []
  | head oldst > head hidst = head oldf : filterNewFiles (tail oldf) (tail oldst) (tail hidst)
  | otherwise               =             filterNewFiles (tail oldf) (tail oldst) (tail hidst)

getFiles :: FilePath -> IO Directory
getFiles dir = do
  contents   <- listDirectory'  dir
  let hiddir                  = dir </> ".tostring"
  createDirectoryIfMissing hiddir
  hidfiles   <- listDirectory' hiddir
  files      <-                 filterM    doesFileExist
                                           contents
  let novafiles               = filter     isPathNova
                                           files
      spfiles                 = filter     isPathUnnova
                                           files
      (oldfiles,newfiles)     = partition (\p -> takeBaseName p `elem` map takeFileName hidfiles)
                                           novafiles
      (maybeReuse, deprfiles) = partition (\p -> takeFileName p `elem` map takeBaseName oldfiles)
                                           hidfiles
  mapM_ removeFile deprfiles
  stamps     <- mapM getModificationTime $ sortBy (comparing takeBaseName) oldfiles
  hidstamps  <- mapM getModificationTime $ sortBy (comparing takeFileName) maybeReuse
  let editedFiles             = filterNewFiles oldfiles stamps hidstamps
  newDirFile <- mapM (\path -> do bs <- C.readFile path
                                  return (File path $ Undone bs))
                     (spfiles ++ editedFiles ++ newfiles)
  maps       <- filterM isPathVisibleDirectory contents
  subdir     <- mapM (\path -> do files <- getFiles path
                                  return (Folder path files))
                      maps
  return (subdir ++ newDirFile)
