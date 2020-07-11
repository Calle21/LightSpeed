module MakeSeq where

makeSeq :: [Token] -> [Token]
makeSeq xs = case head xs of
               SetFileName s -> recTop 1 s (tail xs)
               _             -> error "Expected filename setter for makeSeq"

recTop :: Int -> FilePath -> [Token] -> [Token]
recTop ln filename xs =
  if comesType xs
  then let (xs0,ln0) = toNextLine xs
       in case getok xs0 of
            (2

toNextLine :: [Token] -> ([Token],Int)
toNextLine (SetLine n:xs) = (xs,n)
toNextLine (_:xs)         = toNextLine xs
toNextLine []             = ([],0)
