module List (getFiles) where

 -- import Control.Monad(filterM, liftM, (<=<))
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock(UTCTime)
import Type.DirFile(DirFile(Folder, File), Directory)
import Type.File(File(Undone))
import Ubi
import Util (isVisibleDirectory, listDirectory')
import System.Directory(createDirectoryIfMissing, getModificationTime, removeFile)
import System.FilePath.Posix(takeBaseName, takeExtension, takeFileName, (</>))

getFiles :: FilePath -> IO Directory
getFiles dir = do contents <- listDirectory' dir
                  let novafiles = filter (\p -> takeExtension p == ".nova")
                                          contents
                      fp        = (dir </> ".tostring")
                  createDirectoryIfMissing False fp
                  fconts <- listDirectory' fp
                  let fconts' = filter (\p -> takeBaseName p `elem` map takeBaseName novafiles)
                                        fconts
                  mapM_ (deletes fconts') fconts
                  olddates <- mapM getDate fconts'
                  novafiles' <- filterM (\p -> case lookup (takeBaseName p) olddates of
                                                 Nothing      -> return True -- new file
                                                 Just olddate -> do newdate <- getModificationTime p
                                                                    return (newdate > olddate))
                                         novafiles
                  maps <- filterM isVisibleDirectory contents
                  subdirs <- mapM getFiles maps
                  undones <- mapM (\path -> do bs <- C.readFile path
                                               return (File (path, Undone bs)))
                                   novafiles'
                  return (map Folder subdirs ++ undones)

deletes :: [FilePath] -> FilePath -> IO ()
deletes new old = if old `elem` new
                  then return ()
                  else removeFile old

getDate :: FilePath -> IO (String, UTCTime)
getDate p = do date <- getModificationTime p
               return (takeBaseName p, date)
