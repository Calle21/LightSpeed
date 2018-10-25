module List where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi
import Util

isPathNova   p = C.pack (takeExtension p) == C.pack ".nova"

isPathUnnova p = C.pack (takeFileName p) `arrayElem` Unnovas

Unnovas        = packit [".use",".chain"]

filterNewFiles :: [FilePath] -> [UTCTime] -> [UTCTime] -> [FilePath]
filterNewFiles oldf oldst hidst
  | null oldf               = []
  | head oldst > head hidst = head oldf : filterNewFiles (tail oldf) (tail oldst) (tail hidst)
  | otherwise               =             filterNewFiles (tail oldf) (tail oldst) (tail hidst)

getFiles :: FilePath -> FilePath -> IO Directory
getFiles dir = do
  contents <- listDirectory'  dir
  hidfiles <- listDirectory' (dir </> ".tostring")
  let (files,maps)            = partition  doesFileExist
                                           contents
      novafiles               = filter     isPathNova
                                           files
      spfiles                 = filter     isPathUnnova
                                           files
      (oldfiles,newfiles)     = partition (\p -> takeBaseName p `elem` map takeFileName hidfiles)
                                           novafiles
      (maybeReuse, deprfiles) = partition (\p -> takeFileName p `elem` map takeBaseName oldfiles)
                                           hidfiles
  mapM_ removeFile deprfiles
  stamps    <- mapM getModificationTime $ sortBy (comparing getBaseName) oldfiles
  hidstamps <- mapM getModificationTime $ sortBy (comparing getFileName) maybeReuse
  let editedFiles = filterNewFiles oldfiles stamps hidstamps
  newbs <- mapM C.readFile (spfiles ++ editedFiles ++ newfiles)
  let newDirFile = map (File . Undone) newbs
  subdir <- mapM getFiles maps
  return (subdir ++ newDirFile)
