module Type.DirM(DirM) where

import Type.CompFile

type DirM = CompFile -> IO ()
