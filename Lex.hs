module Lex (lex) where

import qualified Data.ByteString.Char8 as Fast
 -- import Data.Char(isLower, isUpper)
 -- import Data.Function((&))
 -- import Data.List(isPrefixOf)
 -- import Data.Maybe(fromJust)
import Error
import Indent
import Syntax
import Type.CompFN
import Type.File(File(Lexed, Undone))
import Type.Lex
import Type.Token
import Ubi
import Util(charHex, isHex, (|||), subseq, tailinit)
import Prelude hiding (lex)
import Text.Regex.PCRE((=~))

lex :: CompFN
lex _ (path, Undone s) = Lexed $ loop 1 1 s
  where
  loop :: Int -> Int -> Fast.ByteString -> [Lex]
  loop col line inp
    | Fast.null inp = []
    | otherwise = case Fast.head inp of
                    ' '  -> let (n, inp') = countUntil (/=' ') inp
                            in loop (col + n) line inp'
                    '\n' -> let (n, inp') = countUntil (/='\n') inp
                            in loop 1 (line + n) inp'
                    c    -> if Fast.pack "\\\\" `Fast.isPrefixOf` inp then loop col line $ Fast.dropWhile (/='\n') inp
                            else let (tok, len, inp') = token c
                                 in (col, line, tok) : loop (col + len) line inp'
    where
    countUntil :: (Char -> Bool) -> Fast.ByteString -> (Int, Fast.ByteString)
    countUntil p inp = loop 0
      where
      loop n
        | n == inplen          = (n, Fast.empty)
        | Fast.index inp n & p = (n, Fast.drop n inp)
        | otherwise            = loop (n + 1)
      inplen = Fast.length inp
    token :: Char -> (Token, Int, Fast.ByteString)
    token c = if punctuation c then (Punctuation c, 1, Fast.tail inp)
              else case c of
                '"'  -> let start = Fast.tail inp
                        in case Fast.elemIndex '"' start of
                             Nothing -> lError col line path "Couldn't find end of string"
                             Just i  -> let sbs = Fast.take i start
                                            s   = buildString sbs
                                        in (TokenString s, length s, Fast.drop (i + 1) start)
                '\'' -> let (bs, inp')                  = Fast.span (symChar ||| punctuation) (Fast.tail inp)
                            s = Fast.unpack bs
                            c | length s == 1           = head s
                              | s `elem` (fst `map` charwords) = fromJust $ s `lookup` charwords
                              | s == ""                 = if Fast.null inp' then lError (col + 1) line path "Expected something"
                                                                            else if Fast.head inp' == ' ' then ' '
                                                                                 else lError (col + 1) line path "Hmm..."
                              | s =~ "^[0-9a-zA-Z]{2}$" = getHexChar s
                              | otherwise               = lError col line path ("Couldn't read character : '" ++ s)
                        in (TokenChar c, 1 + Fast.length bs, inp')
                _    -> let (bs, inp')                                     = Fast.span symChar inp
                            s = Fast.unpack bs
                            sym | null s                                   = lError col line path ("Illegal character : " ++ [c])
                                | reserved s                               = Reserved s
                                | names s                                  = Keyword s
                                | types s                                  = Type s
                                | s =~ ("^[\\\\" ++ specialChars ++ "]+$") = Opname s
                                | s =~ "^-?\\d+$"                          = TokenInt $ read s
                                | s =~ "^[A-Z]+$"                          = Vartype s
                                | s =~ "^-?\\d+\\.\\d+$"                   = TokenFloat $ read s
                                | s =~ "^-[A-Z0-9]+-$"                     = Special $ tailinit s
                                | s =~ "^%[a-z]{2..}%$"                    = Option  $ tailinit s
                                | s =~ "^`[a-z]{2..}`$"                    = Opname  $ tailinit s
                                | otherwise                                = lError col line path ("Bad token : " ++ s)
                        in (sym, Fast.length bs, inp')
      where
      punctuation :: Char -> Bool
      punctuation c = c `elem` "()[]{}.,@#$"
      charwords = [("nul"  ,   '\NUL'),
                   ("tab"  ,   '\t'),
                   ("newline", '\n'),
                   ("space",   ' ')]
      names, types :: String -> Bool
      names s = s =~ "^[a-z0-9]*'?$" && any isLower s
      types s = s =~ "^[A-Z][a-zA-Z0-9]*$" && not (all isUpper s)
      buildString :: Fast.ByteString -> String
      buildString sbs = loop 0
        where
        loop :: Int -> String
        loop n | n == sbslen                = []
               | sbs `Fast.index` n == '\\' = if n + 1 == sbslen then lError (col + n + 2) line path "String ended in \\"
                                              else if elem (sbs `Fast.index` 1) "nt\\\""
                                                   then (case sbs `Fast.index` 1 of
                                                           'n'  -> '\n'
                                                           't'  -> '\t'
                                                           '\\' -> '\\'
                                                           '"'  -> '"') : loop (n + 2)
                                                   else if n + 2 == sbslen then lError (col + n + 3) line path "Oops..."
                                                        else let s = Fast.unpack (subseq sbs (n + 1) (n + 3))
                                                             in if all isHex s
                                                                then getHexChar s : loop (n + 3)
                                                                else lError (col + n + 2) line path "Hmm..."
               | otherwise                  = sbs `Fast.index` n : loop n
        sbslen = Fast.length sbs

getHexChar :: String -> Char
getHexChar s = let [c0, c1] = map charHex s
               in toEnum $ c0 * 16 + c1

{-
lexline :: String -> [Indent]
lexline s = let (_, Indent y) = indent $ leX ("$line$", s)
            in [y] -}
