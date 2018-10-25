module Lex where

import qualified Data.ByteString.Char8 as BS
import Error
import Syntax
import Types
import Ubi
import Util

novalex :: CompFN
novalex _ (path, Undone s) = (Lexed $ loop 1 1 s, [])
  where
  loop :: Int -> Int -> BS.ByteString -> [Tok2]
  loop col line inp
    | BS.null inp = []
    | otherwise = case BS.head inp of
                    ' '  -> let (n, inp') = countUntil (/=' ') inp
                            in loop (col + n) line inp'
                    '\n' -> let (n, inp') = countUntil (/='\n') inp
                            in loop 1 (line + n) inp'
                    c    -> if BS.pack "\\\\" `BS.isPrefixOf` inp then loop col line $ BS.dropWhile (/='\n') inp
                            else let (tok, len, inp') = token c
                                 in (col, line, tok) : loop (col + len) line inp'
    where
    countUntil :: (Char -> Bool) -> BS.ByteString -> (Int, BS.ByteString)
    countUntil p inp = loop 0
      where
      loop n
        | n == inplen          = (n, BS.empty)
        | BS.index inp n & p = (n, BS.drop n inp)
        | otherwise            = loop (n + 1)
      inplen = BS.length inp
    token :: Char -> (Tok, Int, BS.ByteString)
    token c = if punctuation c then (Punctuation c, 1, BS.tail inp)
              else case c of
                '"'  -> let start = BS.tail inp
                        in case BS.elemIndex '"' start of
                             Nothing -> lError col line path "Couldn't find end of string"
                             Just i  -> let sb = BS.take i start
                                            s  = buildString sb
                                            s' = BS.pack s
                                        in (TokString s', BS.length s', BS.drop (i + 1) start)
                '\'' -> let (bs, inp')                  = BS.span (symChar ||| punctuation) (BS.tail inp)
                            s = BS.unpack bs
                            c | length s == 1           = head s
                              | s `elem` (fst `map` charwords) = fromJust $ s `lookup` charwords
                              | s == ""                 = if BS.null inp' then lError (col + 1) line path "Expected something"
                                                                            else if BS.head inp' == ' ' then ' '
                                                                                 else lError (col + 1) line path "Hmm..."
                              | s =~ "^[0-9a-zA-Z]{2}$" = getHexChar s
                              | otherwise               = lError col line path ("Couldn't read character : '" ++ s)
                        in (TokChar c, 1 + BS.length bs, inp')
                _    -> let (s', inp')             = BS.span symChar inp
                            sym | BS.null s'       = lError col line path ("Illegal character : " ++ [c])
                                | reserved s'      = Reserved s'
                                | synKeyword s'    = Keyword s'
                                | synType s'       = Type s'
                                | synOpname s'     = Opname s'
                                | synInt s'        = TokInt $ read $ BS.unpack s'
                                | synVartype s'    = Vartype s'
                                | synFloat s'      = TokFloat $ read $ BS.unpack s'
                                | synSpecial s'    = Special $ tailinit' s'
                                | synOption s'     = Option  $ tailinit' s'
                                | synOpnameText s' = Opname  $ tailinit' s'
                                | otherwise        = lError col line path ("Bad token : " ++ BS.unpack s')
                        in (sym, BS.length s', inp')
      where
      charwords = [("nul"  ,   '\NUL'),
                   ("tab"  ,   '\t'),
                   ("newline", '\n'),
                   ("space",   ' ')]
      buildString :: BS.ByteString -> String
      buildString bs = loop 0
        where
        loop :: Int -> String
        loop n | n                 == bslen = []
               | bs `BS.index` n == '\\'  = if n + 1 == bslen then lError (col + n + 2) line path "String ended in \\"
                                              else if (bs `BS.index` 1) `elem` "nt\\\""
                                                   then (case bs `BS.index` 1 of
                                                           'n'  -> '\n'
                                                           't'  -> '\t'
                                                           '\\' -> '\\'
                                                           '"'  -> '"') : loop (n + 2)
                                                   else if n + 2 == bslen then lError (col + n + 3) line path "Oops..."
                                                        else let s = BS.unpack (subseq bs (n + 1) (n + 3))
                                                             in if all isHex s
                                                                then getHexChar s : loop (n + 3)
                                                                else lError (col + n + 2) line path "Expected two-char hex"
               | otherwise                  = bs `BS.index` n : loop (n + 1)
        bslen = BS.length bs

getHexChar :: String -> Char
getHexChar [c0,c1] = toEnum $ charToHex c0 * 16 + charToHex c1

{-
lexline :: String -> [Indent]
lexline s = let (_, Indent y) = indent $ leX ("$line$", s)
            in [y] -}
