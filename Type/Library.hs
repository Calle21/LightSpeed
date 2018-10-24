module Type.Library (Library
                         , nameOfLibrary
                         , libOfLibrary) where

import Type.Lib

type Library = (String, Lib)

nameOfLibrary = fst
libOfLibrary  = snd
