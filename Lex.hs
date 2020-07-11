module Lex (lex', opSyntax) where

import Data.List (isPrefixOf)
import Text.Regex.PCRE ((=~))
import Share (Token(..))

lex' :: (FilePath, String) -> [Token]
lex' (filename, inp) = SetFileName filename : lexLines 1 1 inp
  where
  lexLines :: Int -> Int -> String -> [Token]
  lexLines _   _  []       = []
  lexLines col ln s@(x:xs)
    | "..." `isPrefixOf` s = SetCol col : Keyword "..." : lexLines (col + 3) ln (drop 3 s)
    | x == '\n'            = Newline : lexLines 1 (ln + 1) xs
    | x == ' '             = lexLines (col + 1) ln xs
    | x == '"'             = undefined
    | punctChar x          = SetCol col : Punct x : lexLines (col + 1) ln xs
    | otherwise            = case span char s of
                               ("--",s') -> lexLines col ln (dropWhile (/='\n') s')
                               ("**",s') -> undefined
                               (t,s')    -> let t' | nameSyntax t    = Name t
                                                   | opSyntax t      = Op t
                                                   | intSyntax t     = Int $ read t
                                                   | floatSyntax t   = Float $ read t
                                                   | capSyntax t     = Cap t
                                                   | allCapSyntax t  = AllCap t
                                                   | charSyntax t    = Char $ getChar' t
                                                   | loopSyntax t    = Loop $ tail t
                                                   | keywordSyntax t = Keyword t
                                                   | otherwise       = error' col ln filename ("Bad token: " ++ t)
                                            in SetCol col : t' : lexLines (col + length t) ln s'

 -- Helper

getChar' :: String -> Char
getChar' (_:xs) | length xs == 1  = head xs
               | xs == "nul"     = '\0'
               | xs == "tab"     = '\t'
               | xs == "newline" = '\n'
               | xs == "space"   = ' '
               | otherwise       = toEnum (read xs :: Int) -- Problem?

 -- Character predicates

char :: Char -> Bool
char c = c >= 'a' && c <= 'z'
      || c >= 'A' && c <= 'Z'
      || c >= '0' && c <= '9'
      || opChar c || c `elem` "@`"

opChar :: Char -> Bool
opChar c = c `elem` "!$%&/=+?\\^~'*-_:;"

punctChar :: Char -> Bool
punctChar c = c `elem` "#{([)]}.|"

 -- Syntax

loopSyntax :: String -> Bool
loopSyntax s = s =~ "^@[a-z0-9_][a-zA-Z0-9_]*$"

nameSyntax :: String -> Bool
nameSyntax s   = s =~ "^[a-z0-9_][a-zA-Z0-9_]*$"

opSyntax :: String -> Bool
opSyntax s     = (not (null s) && all opChar s) || s =~ "^`[a-zA-Z0-9]+`$"

intSyntax :: String -> Bool
intSyntax s    = s =~ "^[0-9]+$"

floatSyntax :: String -> Bool
floatSyntax s  = s =~ "^[0-9]+\\.[0-9]+$"

capSyntax :: String -> Bool
capSyntax s    = (s =~ "^[A-Z][a-zA-Z0-9_]+$") && not (s =~ "^[A-Z]*$")

allCapSyntax :: String -> Bool
allCapSyntax s = s =~ "^[A-Z]+$"

charSyntax :: String -> Bool
charSyntax s   = s =~ "^'(.|nul|tab|newline|space|([0-9]{3}))$"

keywordSyntax s = s `elem` ["type","synonym","struct","union",">>"
                          , "->", "tcase", "switch", "as", "_", "case", "from"
                          , "range", "lambda", "action", "\\", "the", "="]

 -- Error

error' :: Int -> Int -> FilePath -> String -> a
error' col line filename message = error ("Lex error on line " ++ show line
                               ++ " column " ++ show col ++ " in file "
                               ++ filename ++ ".\n   " ++ message)
