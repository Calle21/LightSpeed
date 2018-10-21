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

charHex :: Char -> Int
charHex c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
          | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a'
          | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A'
          | otherwise            = error "Not numerable"

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

type DirM = File -> IO ()

mapDirM :: DirM -> Directory -> IO ()
mapDirM act (x:xs) = case x of
                       File cf    -> do putStrLn (fst cf)
                                        act (snd cf)
                       Folder dir -> do mapDirM act dir
                                        mapDirM act xs
mapDirM _   []     = return ()

mapWI :: (Int -> a -> b) -> [a] -> [b]
mapWI = rec 0
 where rec i f (x:xs) = f i x : rec (i + 1) f xs
       rec _ _ []     = []

none :: (a -> Bool) -> [a] -> Bool
none pred = all (not . pred)

infixr 2 `or`

or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f0 `or` f1 = (\x -> f0 x || f1 x)

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
                "use",
                "vstruct"]

split :: (Eq a) => a -> [a] -> [[a]]
split elt xs = let (f,r) = break (==elt) xs
               in if null r then [f]
                  else f : split elt (tail r)

tags :: String -> Bool
tags s = s =~ "[a-z]+"

tailinit :: [a] -> [a]
tailinit = init . tail
