module Infix (readInfixFile) where

import Lex (opSyntax)
import Types (Inf(..))

readInfixFile :: String -> [(String,Inf)]
readInfixFile s = read' 1 (map words $ lines s)

read' :: Int -> [[String]] -> [(String,Inf)]
read' _  []      = []
read' ln ([]:xs) = read' (ln + 1) xs
read' ln (x:xs)  = readLine x : read' (ln + 1) xs
  where
  readLine :: [String] -> (String,Inf)
  readLine [a,b,c] = let a' = checkA a
                         b' = checkB $ read b
                         c' = checkC c
                     in (c',Inf a' b')
    where
    checkA :: String -> Char
    checkA "infixr" = 'r'
    checkA "infixl" = 'l'
    checkA s        = error ("Syntax error on line " ++ show ln
                        ++ " in infix file. Bad token: " ++ s)
    checkB :: Int -> Int
    checkB i
      | i >= 0 && i <= 9 = i
      | otherwise        = error ("Syntax error on line " ++ show ln
                                ++ " in infix file. Bad integer: " ++ show i
                                ++ "\nShould be in range 0 to 9.")
    checkC :: String -> String
    checkC s
      | opSyntax s = s
      | otherwise  = error ("Syntax error on line " ++ show ln
                            ++ " in infix file. Not an operator name: "
                            ++ s)
  readLine xs      = error ("Syntax error on line " ++ show ln
                        ++ " in infix file. Expected 3 tokens. Got "
                        ++ show (length xs) ++ ".")
