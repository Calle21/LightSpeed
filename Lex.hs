module Lex (lex') where

import Text.Regex.PCRE ((=~))
import Share (Token(..))
import Unsafe.Coerce (unsafeCoerce)

data Lex = EOF
         | LexAll String
         | LexCap String
         | LexChar Char
         | LexHash Int
         | LexInt Int64
         | LexKeyword String
         | LexLoop String
         | LexName String
         | LexOp String
         | LexString String
         | LexPipe
         | LexPunct Char
         | SetCol Int
         | SetFileName String
         | SetLine Int
         deriving (Show)

lex' :: (FilePath, String) -> [Lex]
lex' (filename, inp) = SetFileName filename : lexLines 1 1 inp
  where
  lexLines :: Int -> Int -> String -> [Token]
  lexLines _   _  []       = []
  lexLines col ln s@(x:xs)
    | x == '\n'            = SetLine (ln + 1) : lexLines 1 (ln + 1) xs
    | x == ' '             = lexLines (col + 1) ln xs
    | x == '"'             = let (stl,st,s') = getString 1 xs
                             in SetCol col : LexString st : lexLines (col + stl) ln s'
    | punctChar x          = SetCol col : Punct x : lexLines (col + 1) ln xs
    | char x               = case span char s of
                               ("--",s') -> lexLines 0 ln $ dropWhile (/='\n') s'
                               ("**",s') -> readMultiline (col + 2) ln s'
                               (ts,s')   -> let t | nameSyntax ts    = LexName ts
                                                  | opSyntax ts      = LexOp ts
                                                  | keywordSyntax ts = LexKeyword ts
                                                  | intSyntax ts     = LexInt $ read ts
                                                  | capSyntax ts     = LexCap ts
                                                  | allCapSyntax ts  = LexAll ts
                                                  | charSyntax ts    = LexChar $ getChar' $ tail ts
                                                  | pipeSyntax ts    = LexPipe
                                                  | loopSyntax ts    = LexLoop $ tail ts
                                                  | nameOpSyntax ts  = LexOp $ init $ tail ts
                                                  | hashSyntax ts    = LexHash $ read $ tail ts
                                                  | otherwise        = error' col ln filename ("Bad token: " ++ ts)
                                            in SetCol col : t : lexLines (col + length ts) ln s'
    | otherwise            = error' col ln filename "Unexpected character"
    where
    getString -> Int -> String -> (Int,String,String)
    getString len (x:xs) | x == '"'  = (len + 1,"",xs)
                         | x == '\\' = case xs of
                                         ('\\':xs') -> go 2 '\\' xs'
                                         ('"':xs')  -> go 2 '"' xs'
                                         ('n':xs')  -> go 2 '\n' xs'
                                         ('t':xs')  -> go 2 '\t' xs'
                                         (_:_)      -> error' (col + len) ln filename "Bad escape char"
                                         []         -> error' (col + len) ln filename "File ended in escape char"
                         | x == '\n' = error' (col + len) ln filename "Literal newlines not allowed in string literals. Use \\n instead"
                         | otherwise = go 1 x xs
      where
      go :: Int -> Char -> String -> (Int,String,String)
      go charlen c xs let (len',rest,xs') = getString (len + charlen) xs
                      in (len',c : rest,xs')
    getString len [] = error (col + len) ln filename "File ended in string literal"
    readMultiline :: Int -> Int -> String -> [Token]
    readMultiline col ln xs
      | "**" `isPrefixOf` xs = lexLines (col + 2) ln $ drop 2 xs
      | "\n" `isPrefixOf` xs = readMultiline 1 (ln + 1) $ tail xs
      | null xs              = error' col ln filename "File ended in multiline comment"
      | otherwise            = readMultiline (col + 1) ln $ tail xs
    getChar' :: String -> Char
    getChar' cs | length cs == 1  = head cs
                | cs == "nul"     = '\0'
                | cs == "tab"     = '\t'
                | cs == "newline" = '\n'
                | cs == "space"   = ' '
                | otherwise       = let i = read cs :: Int
                                    in if i < 256 && i >= 0 then toEnum i
                                       else error' col ln filename "Bad character"

 -- Character predicates

char :: Char -> Bool
char c | c >= 'a' && c <= 'z' = True
       | c >= 'A' && c <= 'Z' = True
       | c >= '0' && c <= '9' = True
       | opChar c             = True
       | otherwise            = c `elem` "`_"

opChar :: Char -> Bool
opChar c = c `elem` "!\"@#$%&/=?+\\^~*-:;<>|"

punctChar :: Char -> Bool
punctChar c = c `elem` "()[]{},.'"

 -- Syntax

nameSyntax :: String -> Bool
nameSyntax s = s =~ "^[0-9]*[a-z][a-zA-Z0-9]*$" && not (keywordSyntax s)

opSyntax :: String -> Bool
opSyntax s = all opChar s && not (keywordSyntax s)

keywordSyntax s = s `elem` [">>"
                          , "\\"
                          , "->"
                          , "<-"
                          , "_"
                          , "="
                          , "as"
                          , "case"
                          , "class"
                          , "do"
                          , "each"
                          , "enum"
                          , "for"
                          , "from"
                          , "if"
                          , "infixl"
                          , "infixr"
                          , "lambda"
                          , "local"
                          , "match"
                          , "modify"
                          , "parallel"
                          , "pure"
                          , "range"
                          , "rec"
                          , "static"
                          , "struct"
                          , "switch"
                          , "synonym"
                          , "tcase"
                          , "the"
                          , "type"
                          , "union"
                          , "until"
                          , "use"
                          , "while"]

intSyntax :: String -> Bool
intSyntax s = s =~ "^-?[0-9]+$"

capSyntax :: String -> Bool
capSyntax s = s =~ "^[0-9]*[A-Z][a-zA-Z0-9]+$" && not (allCapSyntax s)

allCapSyntax :: String -> Bool
allCapSyntax s = s =~ "^[A-Z]+$"

charSyntax :: String -> Bool
charSyntax s = s =~ "^\\\\([^ \\n\\t\\x00]|space|newline|tab|nul|[0-9]{3})$"

pipeSyntax :: String -> Bool
pipeSyntax s = s == "\\\\|"

loopSyntax :: String -> Bool
loopSyntax s = s =~ "^@[0-9]*[a-z][a-zA-Z0-9]*$"

nameOpSyntax :: String -> Bool
nameOpSyntax s = s =~ "^`[0-9]*[a-z][a-zA-Z0-9]*`$"

hashSyntax :: String -> Bool
hashSyntax s = s =~ "^#[0-9]+$"

 -- Error

error' :: Int -> Int -> FilePath -> String -> a
error' col line filename message = error ("Lex error on line " ++ show line
                               ++ " column " ++ show col ++ " in file "
                               ++ filename ++ ".\n   " ++ message)
