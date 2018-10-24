module Type.Pattern (Pattern(..), patternCompat) where

import Type.Type
import Type.Pat
import Type.Setup

newtype Pattern = Pattern (Type, [Pat]) deriving (Ord, Read, Show)

patternCompat :: Setup -> Pattern -> Pattern -> Bool
patternCompat setup (ret0, pat0) (ret1, pat1) = typeCompat setup ret0 ret1 && every (uncurry patCompat) (pat0 `zip` pat1)
