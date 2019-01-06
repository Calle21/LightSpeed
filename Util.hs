module Util where

import Data.Array
import qualified Data.ByteString.Char8 as C
import Ubi

 -- and

infixr 9 &&&

p0 &&& p1 = (\arg -> p0 arg && p1 arg)

 -- all

all' :: (Char -> Bool) -> C.ByteString -> Bool
all' fn bs = loop 0
  where
  loop n | n == C.length bs    = True
         | fn (bs `C.index` n) = loop (n + 1)
         | otherwise           = False

 -- arrayElem

infix 4 `arrayElem`

arrayElem :: Eq a => a -> Array Int a -> Bool
elt `arrayElem` arr = loop i0
  where
  loop i | i > i1         = False
         | arr ! i == elt = True
         | otherwise      = loop (i + 1)
  (i0,i1) = bounds arr

 -- charInBase

charInBase :: Int -> Char -> Bool
charInBase base c = charInt 36 c < base

 -- charInt

charInt :: Int -> Char -> Either String Int
charInt base c = let n | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
                       | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a'
                       | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A'
                       | otherwise            = Left "Not numerable"
                 in if n >= base
                    then Left "Out of base"
                    else Right n

 -- deleteIf

deleteIf :: (a -> Bool) -> [a] -> [a]
deleteIf fn xs = if any fn xs
                 then deleteIf' fn xs
                 else xs

deleteIf' :: (a -> Bool) -> [a] -> [a]
deleteIf' fn (x:xs) | fn x      = xs
                    | otherwise = x : deleteIf' fn xs
deleteIf' _  []     = []

 -- countAndDropUntil

countAndDropUntil :: (Char -> Bool) -> C.ByteString -> (Int, C.ByteString)
countAndDropUntil fn inp = loop 0
  where
  loop n
    | n == C.length inp    = (n, C.empty)
    | fn (inp `C.index` n) = (n, C.drop n inp)
    | otherwise            = loop (n + 1)

 -- dropLine

dropLine :: C.ByteString -> Maybe C.ByteString
dropLine inp = let n = C.elemIndex '\n' inp
               in case n of
                    Nothing -> Nothing
                    Just n' -> Just $ C.drop (n' + 1) inp

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

 -- getSign

getSign :: C.ByteString -> (Int, C.ByteString)
getSign s' = if C.head s' == '-'
             then (-1, C.tail s')
             else (1, s')

 -- hexToInt

hexToInt :: Char -> Int
hexToInt c = case charInt 16 c of
               Right i -> i
               Left _  -> error "Not a hex char"

 -- isHexChar

isHexChar :: Char -> Bool
isHexChar c = case charInt 16 c of
                Right _ -> True
                Left  _ -> False

 -- isPathVisible

isPathVisible :: FilePath -> Bool
isPathVisible path = not (head (takeFileName path) == '.')

 -- isPathVisibleDirectory

isPathVisibleDirectory :: FilePath -> IO Bool
isPathVisibleDirectory path = do ex <- doesDirectoryExist path
                                 return (ex && isPathVisible path)

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

 -- readEscape

readEscape :: String -> (Char,String)
readEscape (_:e:xs) = let c = case e of
                                't'  -> '\t'
                                'n'  -> '\n'
                                '"'  -> '"'
                                '\\' -> '\\'
                      in (c,xs)

 -- readHex

readHex :: String -> (Char,String)
readHex (_:a:b:xs) = (toEnum $ digitToInt a * 16 + digitToInt b, xs)

 -- readHexChar

readHexChar :: C.ByteString -> Maybe Int
readHexChar bs = if C.length bs == 2 && all' isHexChar bs
                 then Just $ readint 16 bs
                 else Nothing

 -- readint

readint :: Int -> C.ByteString -> Int
readint base bs = if base > 0 && base < 37
                  then let (sign, bs') = getSign bs
                       in sign * loop bs' 0 0
                  else error "Base can only be between 1 and 36"
  where
  loop bs' acc i | i == C.length bs = acc
                 | otherwise        = case charInt base (bs `C.index` i) of
                                        Right n -> loop bs' (acc * base + n, i + 1)
                                        Left s  -> error s

 -- reverse apply

infixl 1 &

(&) :: a -> (a -> b) -> b
(&) = flip ($)

 -- split

split :: (a -> Bool) -> [a] -> [[a]]
split fn xs = let (f,r) = break fn xs
               in if null r then [f]
                  else f : split fn (tail r)

 -- C.ByteString

type C.ByteString = C.ByteString

 -- subseq

subseq :: C.ByteString -> Int -> Int -> C.ByteString
subseq bs start end = C.take (end - start) (C.drop start bs)

 -- tailinit

tailinit  = init   . tail
tailinit' = C.tail . C.init
