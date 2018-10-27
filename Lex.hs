module Lex where

import qualified Data.ByteString.Char8 as C
import Error
import Syntax
import Types
import Ubi
import Util

novalex :: CompFN
novalex path (Undone s') = Lexed $ loop 1 1 s'
  where
  loop :: Int -> Int -> String' -> [Tok2]
  loop col line inp
    | C.null inp = []
    | otherwise  = case C.head inp of
                     ' '  -> let (n, inp') = dropAndCountUntil' (/=' ') inp
                             in loop (col + n) line inp'
                     '\n' -> let (n, inp') = dropAndCountUntil' (/='\n') inp
                             in loop 1 (line + n) inp'
                     c    -> if C.pack "\\\\" `C.isPrefixOf` inp then loop col line $ C.dropWhile (/='\n') inp
                             else let (tok, len, inp') = token c
                                  in (col, line, tok) : loop (col + len) line inp'
    where
    token :: Char -> (Tok, Int, String')
    token c = if punctuation c then (Punctuation c, 1, C.tail inp)
              else case c of
                '"'  -> let start = C.tail inp
                        in case '"' `C.elemIndex` start of
                             Just i -> let sr = C.take i start
                                           s' = C.pack $ buildString sr
                                       in (TokString s', C.length s', C.drop (i + 1) start)
                             _      -> lError col line path "Couldn't find end of string"
                '\'' -> let (s', inp') = C.span (symChar ||| punctuation) (C.tail inp)
                            c | C.length   s' == 1 = C.head       s'
                              | charword   s'      = charwordChar s'
                              | C.null     s'      = if C.null inp'
                                                     then lError (col + 1) line path "Expected something"
                                                     else if C.head inp' == ' '
                                                          then ' '
                                                          else lError (col + 1) line path ("Expected punctuation char, space or symchar after ', not " ++ [C.head inp'])
                              | synHexChar s'      = fromJust $ getHexChar s'
                              | otherwise          = lError col line path ("Couldn't read character : '" ++ C.unpack s)
                        in (TokChar c, 1 + C.length s', inp')
                _    -> let (s', inp') = C.span symChar inp
                            sym | C.null        s' = lError col line path ("Illegal character : " ++ [c])
                                | reserved      s' = Reserved                   s'
                                | synKeyword    s' = Keyword                    s'
                                | synOpname     s' = Opname                     s'
                                | synType       s' = Type                       s'
                                | synInt        s' = TokInt $ read $ C.unpack   s'
                                | synTypevar    s' = Typevar                    s'
                                | synOpnameText s' = Opname  $ tailinit'        s'
                                | synFloat      s' = TokFloat $ read $ C.unpack s'
                                | synSpecial    s' = Special $ tailinit'        s'
                                | synOption     s' = Option  $ tailinit'        s'
                                | otherwise        = lError col line path ("Bad token : " ++ C.unpack s')
                        in (sym, C.length s', inp')
      where
      buildString :: String' -> String
      buildString bs = loop 0
        where
        loop :: Int -> String
        loop n | n == C.length bs       = []
               | bs `C.index` n == '\\' = if n + 1 == C.length bs then lError (col + n + 2) line path "String ended in \\"
                                          else if escapeChar (bs `C.index` 1)
                                               then (case bs `C.index` 1 of
                                                       'n'  -> '\n'
                                                       't'  -> '\t'
                                                       '\\' -> '\\'
                                                       '"'  -> '"') : loop (n + 2)
                                               else if n + 2 == C.length bs then lError (col + n + 1) line path "Bad escape char" 
                                                    else case getHexChar $ take 2 $ drop (n + 1) bs of
                                                           Just c -> c : loop (n + 3)
                                                           _      -> lError (col + n + 1) line path "Bad escape char"
               | otherwise              = bs `C.index` n : loop (n + 1)
