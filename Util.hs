module Util where

import Data.Array
import qualified Data.ByteString.Char8 as C
import Ubi

 -- and

infixr 9 &&&

p0 &&& p1 = (\arg -> p0 arg && p1 arg)

 -- arrayElem

infix 4 `arrayElem`

arrayElem :: Eq a => a -> Array Int a -> Bool
elt `arrayElem` arr = loop i0
  where
  loop i | i > i1         = False
         | arr ! i == elt = True
         | otherwise      = loop (i + 1)
  (i0,i1) = bounds arr

 -- deleteIf

deleteIf :: (a -> Bool) -> [a] -> [a]
deleteIf fn xs = if any fn xs
                 then deleteIf' fn xs
                 else xs

deleteIf' :: (a -> Bool) -> [a] -> [a]
deleteIf' fn (x:xs) | fn x      = xs
                    | otherwise = x : deleteIf' fn xs
deleteIf' _  []     = []

 -- dropUntil

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil fn = dropWhile (not . fn)

 -- format

format :: String -> [String] -> String
format s ss = loop s
  where
  loop (c:cs)
    | c == '%'  = case cs of
                    []        -> error "Format string ended with %"
                    ('%':cs') -> '%' : loop cs'
                    (d:cs')   ->
                      let argnum | not (isDigit d)     = error "Expected digit or % after %"
                                 | otherwise           = digitToInt d
                          argstr | argnum >= length ss = error "Not that many arguments"
                                 | otherwise           = ss !! argnum
                      in argstr ++ loop cs'
    | otherwise = c : loop cs
  loop [] = ""

 -- Hex

charToHex :: Char -> Int
charToHex c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
            | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
            | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
            | otherwise            = error ("Not a hex digit : " ++ [c])


isHex :: Char -> Bool
isHex c | c >= '0' && c <= '9' = True
        | c >= 'a' && c <= 'z' = True
        | c >= 'A' && c <= 'Z' = True
        | otherwise            = False

 -- isVisibleDirectory

isVisibleDirectory :: FilePath -> IO Bool
isVisibleDirectory path = do let begin = head (takeFileName path)
                             dir <- doesDirectoryExist path
                             return (dir && not (begin=='.'))


 -- listDirectory

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path = liftM (map (path </>)) (listDirectory path)

 -- listToArray

listToArray xs = listArray (0,length xs - 1) xs

 -- mapWI

mapWI :: (Int -> a -> b) -> [a] -> [b]
mapWI = rec 0
 where rec i f (x:xs) = f i x : rec (i + 1) f xs
       rec _ _ []     = []

 -- none

none :: Foldable t => (a -> Bool) -> t a -> Bool
none p t = not (any p t)

 -- or

infixr 8 |||

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f0 ||| f1 = (\x -> f0 x || f1 x)

 -- packit

packit ss = listToArray (C.pack `map` ss)

 -- putFirst

putFirst :: (a -> Bool) -> [a] -> [a]
putFirst fn xs = case find fn xs of
                   Nothing -> xs
                   Just x  -> x : deleteIf' fn xs

 -- split

split :: (Eq a) => a -> [a] -> [[a]]
split elt xs = let (f,r) = break (==elt) xs
               in if null r then [f]
                  else f : split elt (tail r)

 -- String'

type String' = C.ByteString

 -- subseq

subseq :: C.ByteString -> Int -> Int -> C.ByteString
subseq bs start end = C.take (end - start) (C.drop start bs)

 -- tailinit

tailinit  = init   . tail
tailinit' = C.tail . C.init

 -- and

infixl 1 &

(&) :: a -> (a -> b) -> b
(&) = flip ($)
