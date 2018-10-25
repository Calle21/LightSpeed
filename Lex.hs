module Lex where

import qualified Data.ByteString.Char8 as C
import Error
import Syntax
import Types
import Ubi
import Util

novalex :: CompFN
novalex _ (path, Undone s) = (Lexed $ loop 1 1 s, [])
  where
  loop :: Int -> Int -> String' -> [Tok2]
  loop col line inp
    | C.null inp = []
    | otherwise  = case C.head inp of
                     ' '  -> let (n, inp') = countUntil (/=' ') inp
                             in loop (col + n) line inp'
                     '\n' -> let (n, inp') = countUntil (/='\n') inp
                             in loop 1 (line + n) inp'
                     c    -> if C.pack "\\\\" `C.isPrefixOf` inp then loop col line $ C.dropWhile (/='\n') inp
                             else let (tok, len, inp') = token c
                                  in (col, line, tok) : loop (col + len) line inp'
    where
    countUntil :: (Char -> Bool) -> String' -> (Int, String')
    countUntil p inp = loop 0
      where
      loop n
        | n == inplen          = (n, C.empty)
        | C.index inp n & p = (n, C.drop n inp)
        | otherwise            = loop (n + 1)
      inplen = C.length inp
    token :: Char -> (Tok, Int, String')
    token c = if punctuation c then (Punctuation c, 1, C.tail inp)
              else case c of
                '"'  -> let start = C.tail inp
                        in case C.elemIndex '"' start of
                             Nothing -> lError col line path "Couldn't find end of string"
                             Just i  -> let sb = C.take i start
                                            s  = buildString sb
                                            s' = C.pack s
                                        in (TokString s', C.length s', C.drop (i + 1) start)
                '\'' -> let (bs, inp')                  = C.span (symChar ||| punctuation) (C.tail inp)
                            s = C.unpack bs
                            c | length s == 1           = head s
                              | s `elem` (fst `map` charwords) = fromJust $ s `lookup` charwords
                              | s == ""                 = if C.null inp' then lError (col + 1) line path "Expected something"
                                                                            else if C.head inp' == ' ' then ' '
                                                                                 else lError (col + 1) line path "Hmm..."
                              | s =~ "^[0-9a-zA-Z]{2}$" = getHexChar s
                              | otherwise               = lError col line path ("Couldn't read character : '" ++ s)
                        in (TokChar c, 1 + C.length bs, inp')
                _    -> let (s', inp')             = C.span symChar inp
                            sym | C.null s'       = lError col line path ("Illegal character : " ++ [c])
                                | reserved s'      = Reserved s'
                                | synKeyword s'    = Keyword s'
                                | synType s'       = Type s'
                                | synOpname s'     = Opname s'
                                | synInt s'        = TokInt $ read $ C.unpack s'
                                | synVartype s'    = Vartype s'
                                | synFloat s'      = TokFloat $ read $ C.unpack s'
                                | synSpecial s'    = Special $ tailinit' s'
                                | synOption s'     = Option  $ tailinit' s'
                                | synOpnameText s' = Opname  $ tailinit' s'
                                | otherwise        = lError col line path ("Bad token : " ++ C.unpack s')
                        in (sym, C.length s', inp')
      where
      charwords = [("nul"  ,   '\NUL'),
                   ("tab"  ,   '\t'),
                   ("newline", '\n'),
                   ("space",   ' ')]
      buildString :: String' -> String
      buildString bs = loop 0
        where
        loop :: Int -> String
        loop n | n                 == bslen = []
               | bs `C.index` n == '\\'  = if n + 1 == bslen then lError (col + n + 2) line path "String ended in \\"
                                              else if (bs `C.index` 1) `elem` "nt\\\""
                                                   then (case bs `C.index` 1 of
                                                           'n'  -> '\n'
                                                           't'  -> '\t'
                                                           '\\' -> '\\'
                                                           '"'  -> '"') : loop (n + 2)
                                                   else if n + 2 == bslen then lError (col + n + 3) line path "Oops..."
                                                        else let s = C.unpack (subseq bs (n + 1) (n + 3))
                                                             in if all isHex s
                                                                then getHexChar s : loop (n + 3)
                                                                else lError (col + n + 2) line path "Expected two-char hex"
               | otherwise                  = bs `C.index` n : loop (n + 1)
        bslen = C.length bs

getHexChar :: String -> Char
getHexChar [c0,c1] = toEnum $ charToHex c0 * 16 + charToHex c1

{-
lexline :: String -> [Indent]
lexline s = let (_, Indent y) = indent $ leX ("$line$", s)
            in [y] -}
