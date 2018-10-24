module Ubi (digitToInt
          , filterM
          , find
          , fromJust
          , intercalate
          , intersperse
          , isDigit
          , isLower
          , isPrefixOf
          , isSuffixOf
          , isUpper
          , liftM
          , (<=<)
          , (&)) where

import Control.Monad(filterM, liftM, (<=<))
import Data.Char (digitToInt, isDigit, isLower, isUpper)
import Data.Function((&))
import Data.List (find, intercalate, intersperse, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)

 -- tags :: String -> Bool
 -- tags s = s =~ "[a-z]+"
