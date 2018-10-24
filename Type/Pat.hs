module Type.Pat (Pat(..), patCompat) where

import Type.Type
import Type.Setup

data Pat = PatK String
         | PatT Type
         deriving (Ord, Read, Show)

patCompat :: Setup -> Pat -> Pat -> Bool
patCompat _     (PatK s0) (PatK s1) = s0 == s1
patCompat _     (PatK _)  (PatT _)  = True
patCompat setup (PatT t0) (PatT t1) = typeCompat setup t0 t1
patCompat _     _         _         = False

getTypeAsTuple :: [Pat] -> Type
getTypeAsTuple pat = typeConcat $ filterIt pat
  where
  filterIt ((PatT t):xs) = t : filterIt xs
  filterIt ((PatK _):xs) = filterIt xs
  filterIt []            = []
