module Lex where

import qualified Data.ByteString.Char8 as C
import Error
import Syntax
import Types
import Ubi
import Util

lex' :: CompFN
lex' path (Undone s) = Lexed $ loop 1 1 s
  where
  loop :: Int -> Int -> C.ByteString -> [Tok2]
  loop col line inp
    | C.null inp = []
    | otherwise  = if col == 1 && multiline `C.isPrefixOf` inp then multiLine (line + 1) (C.drop 22 inp)
                   else case C.head inp of
                          ' '  -> let (n, inp') = countAndDropUntil (/=' ') inp
                                  in loop (col + n) line inp'
                          '\n' -> let (n, inp') = countAndDropUntil (/='\n') inp
                                  in loop 1 (line + n) inp'
                          c    -> if C.pack "\\\\" `C.isPrefixOf` inp then loop col line $ C.dropWhile (/='\n') inp
                                  else let (tok, len, inp') = token c
                                       in (col, line, tok) : loop (col + len) line inp'
    where
    multiLine :: C.ByteString -> [Tok2]
    multiLine line inp | multiline `C.isPrefixOf` inp = loop 1 (line + 1) (C.drop 22 inp)
                       | otherwise                    = let inp' = dropLine inp
                                                        in case inp' of
                                                             Nothing    -> lError (-1) line "File ended in multiline comment"
                                                             Just inp'' -> multiLine (line + 1) inp''
    token :: Char -> (Tok, Int, C.ByteString)
    token c = if punctuation c then (Punctuation c, 1, C.tail inp)
              else case c of
                '"'  -> let start = C.tail inp
                        in case '"' `C.elemIndex` start of
                             Just i -> let sr = C.unpack $ C.take i start
                                           s' = C.pack $ readString sr
                                       in (TokString s', i + 2, C.drop (i + 1) start)
                             _      -> lError col line path "Couldn't find end of string"
                '\'' -> let (s', inp') = C.span (symChar ||| punctuation) (C.tail inp)
                            c | C.length s' == 1   = C.head s'
                              | charword s'        = charwordToChar s'
                              | C.null s'          = if C.null inp'
                                                     then lError (col + 1) line path "Expected something"
                                                     else if C.head inp' == ' '
                                                          then ' '
                                                          else lError (col + 1) line path "Bad char"
                              | synHexChar s'      = toEnum $ readint 16 s'
                              | otherwise          = lError col line path "Bad char"
                        in (TokChar c, 1 + C.length s', inp')
                _    -> let (s', inp')             = C.span symChar inp
                            sym | C.null s'        = lError col line path ("Illegal character : " ++ [c])
                                | reserved s'      = Reserved s'
                                | synKeyword s'    = Keyword s'
                                | synOpname s'     = Opname s'
                                | synType s'       = Type s'
                                | synInt s'        = TokInt $ readint 10 s'
                                | synTypevar s'    = Typevar s'
                                | synOpnameText s' = Opname $ tailinit' s'
                                | synFloat s'      = TokFloat $ read $ C.unpack s'
                                | synRatio s'      = let [num,den] = C.split '/' s'
                                                     in TokRatio (readint 10 num) (readint 10 den)
                                | synSpecial s'    = Special $ tailinit' s'
                                | synOption s'     = Option  $ tailinit' s'
                                | otherwise        = lError col line path ("Bad token : " ++ C.unpack s')
                        in (sym, C.length s', inp')
      where
      readString :: String -> String
      readString [] = []
      readString xs | xs =~ "^\\\\[tn\"\\\\]"     = let (c,xs') = readEscape xs
                                                    in c : readString xs'
                    | xs =~ "^\\\\[a-fA-F0-9]{2}" = let (c,xs') = readHex xs
                                                    in c : readString xs'
                    | otherwise                   = head xs : readString (tail xs)
