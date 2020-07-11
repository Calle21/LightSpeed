module Simplify (simplify) where

import Types (Token(..))

simplify :: (FilePath, [(Int, [(Int,Token)])] -> (FilePath, [(Int, [Token])])
simplify (filename, xs) = (filename, rec xs)

rec xs = 
