module Type.DirFile (DirFile(..)
                         , Directory
                         , countFiles
                         , mapDir
                         , mapDirM) where

import Type.CompFile
import Type.CompFN
import Type.DirM
import Type.Setup

type Directory = [DirFile]

data DirFile = Folder Directory
             | File   CompFile
             deriving (Read, Show)

countFiles :: Directory -> Int
countFiles (x:xs) = case x of
                      File   _   -> 1 + countFiles xs
                      Folder dir -> countFiles dir + countFiles xs
countFiles []     = 0

mapDir :: Setup -> CompFN -> Directory -> Directory
mapDir setup fn (x:xs) = case x of
                           File cf    -> File (fst cf, fn setup cf)   : mapDir setup fn xs
                           Folder dir -> Folder (mapDir setup fn dir) : mapDir setup fn xs
mapDir _     _  []     = []

mapDirM :: DirM -> Directory -> IO ()
mapDirM act (x:xs) = case x of
                       File   cf  -> do act cf
                       Folder dir -> do mapDirM act dir
                                        mapDirM act xs
mapDirM _   []     = return ()
