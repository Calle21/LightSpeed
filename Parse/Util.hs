module Nova.Parse.Util where

import Nova.Ubi

infix 1 `match`

match :: [Tok] -> PMonad -> Bool
xs `match` m = isJust $ m xs

 -- Monads

aFixity :: PMonad
aFixity (x:xs) = case snd x of
                   (Reserved "infixr")  -> one isInt xs
                   (Reserved "infixl")  -> one isInt xs
                   (Reserved "postfix") -> Just xs
                   (Reserved "prefix")  -> Just xs
                   _                    -> Nothing
aFixity []     = Nothing

aType :: PMonad
aType = oneOrMore (isType `or` isVartype)

isEnd :: PMonad
isEnd [] = Just []
isEnd _  = Nothing

isTypeName :: PMonad
isTypeName = one isType >=> zeroOrOne vartype

notEnd :: PMonad
notEnd [] = Nothing
notEnd xs = Just xs

plist :: PMonad
plist = one isStartParen >=> listof (Punct ',') (one (is (Punct ')'))) (aType >=> one isName) >=> one isEndParen

 -- Monad generators

cyclic :: PMonad -> PMonad
cyclic m xs = case m xs of
                Just xs' -> cyclic m xs'
                Nothing  -> Just xs

exactly :: Int -> TokP -> PMonad
exactly 0 _ xs           = xs
exactly n f (x:xs) | f x = exactly (n - 1) f xs
exactly _ _ _            = Nothing

one :: TokP -> PMonad
one p (x:xs) | p x = Just xs
one _ _            = Nothing

oneOrMore :: TokP -> PMonad
oneOrMore f (x:xs) | f x = zeroOrMore f xs
oneOrMore _ _ = Nothing

zeroOrMore :: TokP -> PMonad
zeroOrMore _ [] = Just []
zeroOrMore f xs | f $ head xs = loop $ tail xs
                | otherwise   = Just xs

zeroOrOne :: TokP -> PMonad
zeroOrOne _ [] = Just []
zeroOrOne f xs | f $ head xs = Just $ tail xs
               | otherwise   = Just xs

 -- Lists

listOf :: Token -> PMonad -> PMonad -> PMonad
listOf sep end cont xs = case end xs of
                           Just xs' -> Just xs'
                           Nothing  -> listof1 sep end cont xs

listOf1 :: Token -> PMonad -> PMonad  -> PMonad
listOf1 sep end cont xs = case cont xs of
                            Just xs' -> case one sep $ xs' of
                                          Just xs'' -> listof1 sep end cont xs''
                                          Nothing   -> end xs'
                            Nothing  -> Nothing

listOf1' :: String -> (String -> Bool) -> [String] -> Bool
listOf1' sep cont (x:xs) | cont x    = case xs of
                                         x':xs' -> if x' == sep then listof1' sep cont xs'
                                                   else False
                                         _      -> True
                         | otherwise = False

listOf2 :: Token -> PMonad -> PMonad  -> PMonad
listOf2 sep end cont xs = case cont xs of
                            Just xs' -> case one sep $ xs' of
                                          Just xs'' -> listof1 sep end cont xs''
                                          Nothing   -> Nothing
                            Nothing  -> Nothing

 -- Predicate generators

is :: Token -> TokP
is t = \(_, x) -> x == t

 -- Predicates

isComma :: TokP
isComma (_, Punct ',') = True
isComma _              = False

isConstr = isModS

isEndParen :: TokP
isEndParen (_, Punct ')') = True
isEndParen _              = False

isEqual :: TokP
isEqual (_, Reserved "=") = True
isEqual _                 = False

isFName :: TokP
isFName (_, Name _)   = True
isFName (_, Opname _) = True
isFName _             = False

isInfixDecl :: TokP
isInfixDecl (_, InfixDecl _) = True
isInfixDecl _                = False

isInt :: TokP
isInt (_, PInt _) = True
isInt _           = False

isModS :: TokP
isModS (_, Type _)    = True
isModS (_, Vartype _) = True
isModS _              = False

isName :: TokP
isName (_, Name _) = True
isName _           = False

isOpname :: TokP
isOpname (_, Opname _) = True
isOpname _             = False

isStartParen :: TokP
isStartParen (_, Punct '(') = True
isStartParen _              = False

isTags :: TokP
isTags (_, Name s) = tags s
isTags _           = False

isType :: TokP
isType (_, Type _) = True
isType _           = False

isTypeLang :: TokP
isTypeLang (_,t) = extract t
  where
  extract (Type _)    = True
  extract (Keyword s) = s `elem` ["signed", "unsigned", "<-", "array", "list"]
  extract (Opname s)  = s `elem` ["|"]
  extract (Punct c)   = c `elem` ['(', ')', ',']
  extract (Name _)    = True
  extract (TInt _)    = True

isVartype :: TokP
isVartype (_, Vartype s) = True
isVartype _              = False

 -- Extractors

colOf :: [Tok] -> String -> Int -> Int
colOf ((c,_):_) _ _ = c
colOf _ fname ln    = pError ln fname "Expected more tokens"

fixityOf :: (String, Setup) -> (Int, FilePath) -> Fixity
fixityOf (s,m:ms) (ln,path) = let fixs = fixity m
                              in case s `Map.lookup` fixs of
                                   Just f  -> f
                                   Nothing -> fixityOf (s,ms) (ln,path)
fixityOf (s,[])   (ln,path) = pError ln path ("This operator has no fixity : " ++ s)

followed :: (Indent, [Indent]) -> Int -> ([Indent], [Indent])
followed (previous, follow) level = case follow of
                                      (Indent ys):_ -> if getColumn ys == level then (previous : ys, tail follow)
                                                       else ([previous], follow)
                                      _             -> ([previous], follow)

follows :: [Indent] -> Int -> ([Indent], [Indent])
follows follow level = case follow of
                         (Indent ys):_ -> if getColumn ys == level then (ys, tail follow)
                                          else ([], follow)
                         _             -> ([], follow)

getList :: Token -> PMonad -> [Tok] -> ([[Token]], [Tok])
getList sep end xs = loop acc xs
  where
  loop :: [[Token]] -> [Tok] -> ([[Token]], [Tok])
  loop acc xs = case end xs of
                  Just xs' -> (reverse acc, xs')
                  Nothing  -> let (toks, _:xs') = span (\x -> snd x /= sep) xs
                              in loop (map snd toks : acc) xs'

getList1 :: PMonad -> [Tok] -> ([Token], [Tok])
getList1 end xs = loop acc xs
  where
  loop :: [Token] -> [Tok] -> ([Token], [Tok])
  loop acc xs = case end xs of
                  Just xs' -> (reverse acc, xs')
                  Nothing  -> loop (snd (head xs) : acc) (drop 2 xs)

theLine :: Indent -> String -> (Int, [Tok])
theLine (Line ln xs) _       = (ln, xs)
theLine (Indent ys) filename = pError (getLine $ head ys) filename "Unexpected indentation"

trep :: Tok -> String
trep (_, Name s)        = s
trep (_, Punct ',')     = "|"
trep (_, Reserved "->") = "|"
trep (_, Type s)        = s
trep (_, Vartype s)     = s

 -- Parsing

infix 4 `isAutoCompatible`

isAutoCompatible :: String -> String -> Bool
s0 `isAutoCompatible` s1 = let s0' = if s0 =~ "[a-z]+[0-9]+" then dropUntil isDigit s0 else s0
                           in s0' == s1
