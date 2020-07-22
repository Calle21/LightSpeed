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
    | x == '\n'            = SetLine (ln + 1) : lexLines 1 (ln + 1) xs
    | x == ' '             = lexLines (col + 1) ln xs
    | x == '"'             = undefined
    | punctChar x          = SetCol col : Punct x : lexLines (col + 1) ln xs
    | otherwise            = case span char s of
                               ("--",s') -> lexLines col ln (dropWhile (/='\n') s')
                               ("**",s') -> readMultiline (col + 2) ln s'
                               (t,s')    -> let t' | nameSyntax t    = Name t
                                                   | opSyntax t      = Op t
                                                   | nameOpSyntax t  = Op $ init $ tail t
                                                   | intSyntax t     = Int $ read t
                                                   | floatSyntax t   = Type (LU "Float") $ Int $ unsafeCoerce (read t :: Float)
                                                   | capSyntax t     = Cap t
                                                   | allCapSyntax t  = AllCap t
                                                   | charSyntax t    = Type (LU "Char") (Int $ getChar' $ tail t)
                                                   | loopSyntax t    = Loop $ tail t
                                                   | keywordSyntax t = Keyword t
                                                   | opNameSyntax t  = Name $ init $ tail t
                                                   | otherwise       = error' col ln filename ("Bad token: " ++ t)
                                            in SetCol col : t' : lexLines (col + length t) ln s'
    where
    readMultiline :: Int -> Int -> String -> [Token]
    readMultiline col ln (x0:x1:xs)
      | x0 == '*' && x1 == '*' = lexLines (col + 2) ln xs
      | x0 == '\n'             = readMultiline 1 (ln + 1) (x1:xs)
      | otherwise              = readMultiline (col + 1) ln (x1:xs)
    readMultiline col ln _ = error' col ln filename "File ended in multiline comment"

 -- Helper

getChar' :: String -> Int
getChar' xs | length xs == 1  = fromEnum $ head xs
            | xs == "nul"     = fromEnum '\0'
            | xs == "tab"     = fromEnum '\t'
            | xs == "newline" = fromEnum '\n'
            | xs == "space"   = fromEnum ' '
            | otherwise       = read xs :: Int

 -- Character predicates

alpha :: Char -> Bool
alpha c | c >= 'a' && c <= 'z' = True
        | c >= 'A' && c <= 'Z' = True
        | otherwise            = False

char :: Char -> Bool
char c | alpha c              = True
       | c >= '0' && c <= '9' = True
       | opChar c             = True
       | c `elem` "@`"       = True
       | otherwise            = False

opChar :: Char -> Bool
opChar c = c `elem` "!$%&/=+?\\^~'*-_:"

punctChar :: Char -> Bool
punctChar c = c `elem` "()[]{}.;#"

 -- Syntax

loopSyntax :: String -> Bool
loopSyntax s = s =~ "^@[a-z0-9_][a-zA-Z0-9_]*$" && any alpha s

nameSyntax :: String -> Bool
nameSyntax s = s =~ "^[a-z0-9_][a-zA-Z0-9_]*$" && any alpha s

opSyntax :: String -> Bool
opSyntax s = not (null s) && all opChar s

opNameSyntax :: String -> Bool
opNameSyntax s = head s == '`' && last s == '`'
              && let s' = init $ tail s
                 in not (null s') && all (opChar s')

nameOpSyntax :: String -> Bool
nameOpSyntax s = s =~ "^`[a-zA-Z0-9]+`$" && any alpha s

intSyntax :: String -> Bool
intSyntax s = s =~ "^[0-9]+$"

floatSyntax :: String -> Bool
floatSyntax s = s =~ "^[0-9]+\\.[0-9]+$"

capSyntax :: String -> Bool
capSyntax s = (s =~ "^[A-Z][a-zA-Z0-9_]+$") && not (s =~ "^[A-Z]+$")

allCapSyntax :: String -> Bool
allCapSyntax s = s =~ "^[A-Z]+$"

charSyntax :: String -> Bool
charSyntax s = s =~ "^'(.|nul|tab|newline|space|([0-9]{3}))$"

keywordSyntax s = s `elem` ["|"
                          , ">>"
                          , "<<"
                          , "->"
                          , "<-"
                          , "_"
                          , "\\"
                          , "="
                          , "as"
                          , "case"
                          , "class"
                          , "delay"
                          , "destroy"
                          , "do"
                          , "each"
                          , "enum"
                          , "for"
                          , "from"
                          , "if"
                          , "infixl"
                          , "infixr"
                          , "lambda"
                          , "locals"
                          , "match"
                          , "modify"
                          , "parallel"
                          , "pure"
                          , "range"
                          , "static"
                          , "struct"
                          , "switch"
                          , "synonym"
                          , "tcase"
                          , "the"
                          , "type"
                          , "union"
                          , "use"
                          , "while"]


 -- Error

error' :: Int -> Int -> FilePath -> String -> a
error' col line filename message = error ("Lex error on line " ++ show line
                               ++ " column " ++ show col ++ " in file "
                               ++ filename ++ ".\n   " ++ message)
