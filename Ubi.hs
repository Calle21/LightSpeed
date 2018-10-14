module Nova.Ubi where

import Data.Char (isLower, isUpper)
import Data.List(find, insertBy, intercalate, isPrefixOf, partition, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Word (Word)
import Nova.Error
import Nova.Lex(lexline)
import Nova.Types
import Prelude hiding (getLine, lex, delete)
import Text.Regex.PCRE((=~))

isAnnotation :: String -> Bool
isAnnotation s = s `elem` ["mutable",
                           "static"]

delete' :: (a -> Bool) -> [a] -> [a]
delete' pred (x:xs) | pred x    = xs
                    | otherwise = x : delete' pred xs
delete' _    []     = []

deleteIf :: (a -> Bool) -> [a] -> [a]
deleteIf pred ls = if any pred ls
                   then delete' pred ls
                   else ls

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil pred = dropWhile (not . pred)

mapWI :: (Int -> a -> b) -> [a] -> [b]
mapWI = rec 0
 where rec i f (x:xs) = f i x : rec (i + 1) f xs
       rec _ _ []     = []

none :: (a -> Bool) -> [a] -> Bool
none pred = all (not . pred)

putFirst :: (a -> Bool) -> [a] -> [a]
putFirst pred ls = if any pred ls
                   then find pred ls : delete pred ls
                   else ls

specialFiles = ["autotag",
                "chain",
                "enum",
                "ops",
                "struct",
                "synonym",
                "tag",
                "type",
                "union",
                "use"]

split :: (Eq a) => a -> [a] -> [[a]]
split elt xs = let (f,r) = break (==elt) xs
               in if null r then [f]
                  else f : split elt (tail r)

tags :: String -> Bool
tags s = s =~ "[a-z]+"
