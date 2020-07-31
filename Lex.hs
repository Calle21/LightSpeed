module Lex (lex') where

import Text.Regex.PCRE ((=~))
import Share (Token(..))
import Unsafe.Coerce (unsafeCoerce)

lex' :: (FilePath, String) -> [Token]
lex' (filename, inp) = SetFileName filename : lexLines 1 1 inp
  where
  lexLines :: Int -> Int -> String -> [Token]
  lexLines _   _  []       = []
  lexLines col ln s@(x:xs)
    | x == '\n'            = SetLine (ln + 1) : lexLines 1 (ln + 1) xs
    | x == ' '             = lexLines (col + 1) ln xs
    | x == '"'             = let (stl,st,s') = getString 1 [] xs
                             in SetCol col : Array [length st] (map makeChar st) : lexLines (col + stl) ln s'
    | punctChar x          = SetCol col : Punct x : lexLines (col + 1) ln xs
    | char x               = case span char s of
                               ("--",s') -> lexLines 0 ln $ dropWhile (/='\n') s'
                               ("**",s') -> readMultiline (col + 2) ln s'
                               (ts,s')   -> let t | nameSyntax ts    = Name ts
                                                  | opSyntax ts      = Op ts
                                                  | keywordSyntax ts = Keyword ts
                                                  | intSyntax ts     = Int $ read ts
                                                  | floatSyntax ts   = Type (NM "Float" [])
                                                                            (Int $ unsafeCoerce (read ts :: Double))
                                                  | capSyntax ts     = Cap ts
                                                  | allCapSyntax ts  = AllCap ts
                                                  | charSyntax ts    = makeChar $ getChar' $ tail ts
                                                  | loopSyntax ts    = Loop $ tail ts
                                                  | nameOpSyntax ts  = Op $ init $ tail ts
                                                  | opNameSyntax ts  = Name $ init $ tail ts
                                                  | partSyntax ts    = Part $ read $ tail ts
                                                  | accessSyntax ts  = Access $ splitAccess ts
                                                  | otherwise        = error' col ln filename ("Bad token: " ++ ts)
                                            in SetCol col : t : lexLines (col + length ts) ln s'
    | otherwise            = error' col ln filename "Unexpected character"
    where
    getString -> Int -> String -> String -> (Int,String,String)
    getString len acc (x:xs) | x == '\\' = case xs of
                                             ('\\':xs') -> getString (len + 2) ('\\' : acc) xs'
                                             ('"':xs')  -> getString (len + 2) ('"' : acc) xs'
                                             ('n':xs')  -> getString (len + 2) ('\n' : acc) xs'
                                             ('t':xs')  -> getString (len + 2) ('\t' : acc) xs'
                                             (_:_)      -> error' (col + len) ln filename "Bad escape char"
                                             []         -> error' (col + len) ln filename "File ended in escape char"
                             | x == '"'  = (len + 1, reverse acc, xs)
                             | x == '\n' = error' (col + len) ln filename "Literal newlines not allowed in string literals. Use \\n instead"
                             | otherwise = getString (len + 1) (x : acc) xs
    getString len _   []     = error (col + len) ln filename "File ended in string literal"
    readMultiline :: Int -> Int -> String -> [Token]
    readMultiline col ln (x0:x1:xs)
      | x0 == '*' && x1 == '*' = lexLines (col + 2) ln xs
      | x0 == '\n'             = readMultiline 1 (ln + 1) (x1:xs)
      | otherwise              = readMultiline (col + 1) ln (x1:xs)
    readMultiline col ln _ = error' col ln filename "File ended in multiline comment"
    getChar' :: String -> Char
    getChar' cs | length cs == 1  = head cs
                | cs == "nul"     = '\0'
                | cs == "tab"     = '\t'
                | cs == "newline" = '\n'
                | cs == "space"   = ' '
                | otherwise       = let i = read cs :: Int
                                    in if i < 256 && i >= 0 then toEnum i
                                       else error' col ln filename "Bad character"

 -- Helper

makeChar :: Char -> Token
makeChar c = Type (NM "Char" []) (Int $ fromIntegral $ fromEnum c)

splitAccess :: String -> [String]
splitAccess ts = let (nm,rest) = break (=='.') ts
                 in if null rest then [nm]
                    else nm : splitAccess (tail rest)

 -- Character predicates

char :: Char -> Bool
char c | c >= 'a' && c <= 'z' = True
       | c >= 'A' && c <= 'Z' = True
       | c >= '0' && c <= '9' = True
       | opChar c             = True
       | otherwise            = c `elem` "`_"

opChar :: Char -> Bool
opChar c = c `elem` "!\"@#$%&/=?+\\^~*'-:.;<>|"

punctChar :: Char -> Bool
punctChar c = c `elem` "()[]{},"

 -- Syntax

nameSyntax :: String -> Bool
nameSyntax s = s =~ "^[0-9]*[a-z][a-zA-Z0-9]*$" && not (keywordSyntax s)

opSyntax :: String -> Bool
opSyntax s = all opChar s && not (keywordSyntax s)

keywordSyntax s = s `elem` [">>"
                          , "<<"
                          , "->"
                          , "<-"
                          , "_"
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
                          , "until"
                          , "use"
                          , "while"]

intSyntax :: String -> Bool
intSyntax s = s =~ "^[0-9]+$"

floatSyntax :: String -> Bool
floatSyntax s = "^[0-9]+\\.[0-9]+$"

capSyntax :: String -> Bool
capSyntax s = s =~ "^[0-9]*[A-Z][a-zA-Z0-9]+$" && not (allCapSyntax s)

allCapSyntax :: String -> Bool
allCapSyntax s = s =~ "^[A-Z]+$"

charSyntax :: String -> Bool
charSyntax s = s =~ "^\\\\([^ \\n\\t]|space|newline|tab|nul|[0-9]{3})$"

loopSyntax :: String -> Bool
loopSyntax s = head s == '@' && nameSyntax (tail s)

nameOpSyntax :: String -> Bool
nameOpSyntax s = length s > 2 && head s == '`' && last s == '`' && nameSyntax (init (tail s))

opNameSyntax :: String -> Bool
opNameSyntax s = length s > 2 && head s == '`' && last s == '`' && opSyntax (init (tail s))

partSyntax :: String -> Bool
partSyntax s = s =~ "^#[0-9]+$"

accessSyntax :: String -> Bool
accessSyntax s = all nameSyntax (splitAccess s)

 -- Error

error' :: Int -> Int -> FilePath -> String -> a
error' col line filename message = error ("Lex error on line " ++ show line
                               ++ " column " ++ show col ++ " in file "
                               ++ filename ++ ".\n   " ++ message)
