[Int]module GetProtos (getProtos) where

import Share (comesType)

getProtos :: [Token] -> [Proto]
getProtos xs = case xs of
                 (SetFileName s:xs') -> recTop 1 s xs'
                 _                   -> error "Expected filename setter for getProtos"

recTop :: Int -> FilePath -> [Token] -> [Proto]
recTop ln filename xs = if comesType xs
                        then let (p,xs',ln') = getProto ln filename xs
                             in p : recTop ln' filename xs'
                        else case xs of
                               []                  -> []
                               (SetFileName s:xs') -> recTop 1 s xs'
                               _                   -> let (xs',ln') = skipAhead ln xs
                                                      in recTop ln' filename xs'

getProto :: Int -> FilePath -> [Token] -> (Proto,[Token],Int)
getProto ln filename xs = let (tp0,xs0,ln0) = readArbType ln filename xs
                              (nm,xs1)      = readName ln filename xs0
                              xs2           = readOpenParen ln filename xs1
                              (tp1,xs3,ln3) = readDelimiter ln filename ')' readArgType
                          in Proto tp0 nm tp1 : recTop ln3 filename xs3

readArgType :: Int -> FilePath -> [Token] -> (Token, [Token], Int)
readArgType ln filename xs = let (tp,xs0,ln0) = readArbType ln filename xs
                                 (_,xs1)      = readName xs0
                             in (tp,xs1,ln0)
