module Nova.Lex (lex, lexline) where

import Nova.Indent
import Nova.Syntax
import Nova.Ubi

lex :: CompDir
lex (_, (path, Undone s)) = Lexed $ loop 1 1 s
  where
  loop :: Int -> Int -> String -> [Lex]
  loop col line inp
    | null inp  = []
    | otherwise = case head inp of
                    ' '  -> let (n, inp') = countUntil (/=' ') inp
                            in loop (col + n) line inp'
                    '\n' -> let (n, inp') = countUntil (/='\n') inp
                            in loop 1 (line + n) inp'
                    c    -> if "\\\\" `isPrefixOf` inp then loop col line $ dropWhile (/='\n') inp
                            else let (tok, len, inp') = token c
                                 in (col, line, tok) : loop (col + len) line inp'
    where
    countUntil :: (Char -> Bool) -> String -> (Int, String)
    countUntil = loop 0
      where
      loop n p inp
        | null inp     = (n, [])
        | p $ head inp = (n, inp)
        | otherwise    = loop (n + 1) p (tail inp)
    token :: Char -> (Token, Int, String)
    token c = if punctuation c then (Punct c, 1, tail inp)
              else case c of
                '"'  -> getString "" 1 (tail inp)
                '\'' -> let (s, inp')                       = span (symchar `or` punctuation) $ tail inp
                            c | length s == 2               = s !! 1
                              | s `elem` (fst `map` charwords) = fromJust $ s `lookup` charwords
                              | s        == ""              = if null inp then lError col line path "Expected something"
                                                                          else if head inp == ' ' then ' '
                                                                                                  else lError col line path "Hmm..."
                              | s =~ "^[0-9a-zA-Z]{2}$"     = let [c0,c1] = charHex `map` tail s
                                                              in toEnum $ c0 * 16 + c1
                              | otherwise                   = lError col line path ("Couldn't read character : '" ++ s)
                        in (AChar c, 1 + length s, inp')
                _    -> let (s, inp')                                      = span symChar inp
                            sym | null s                                   = lError col line path ("Illegal character : " ++ [c])
                                | reserved s                               = Reserved s
                                | names s                                  = Name s
                                | types s                                  = Type s
                                | s =~ ("^[\\\\" ++ specialChars ++ "]+$") = Opname s
                                | s =~ "^-?\\d+$"                          = AInt $ read s
                                | s =~ "^[A-Z]+$"                          = Vartype s
                                | s =~ "^-?\\d+\\.\\d+$"                   = AFloat $ read s
                                | s =~ "^\\*[A-Z0-9]+\\*$"                 = Special s
                                | s =~ "^%[a-z]{2..}%$"                    = Option $ tailinit s
                                | s =~ "^`[a-z]+`$"                        = Opname $ tailinit s
                                | otherwise                                = lError col line path ("Bad token : " ++ s)
                        in (sym, length s, inp')
      where
      punctuation :: Char -> Bool
      punctuation c = c `elem` "()[]{}.,@#"
      charwords = [("nul",'\NUL'),
                   ("tab", '\t'),
                   ("newline", '\n'),
                   ("space", ' ')]
      names, types :: String ->     Bool
      names s = s =~ "^[a-z0-9]*'?$" && any isLower s
      types s = s =~ "^[A-Z][a-zA-Z0-9]*$" && not (all isUpper s)
      getString :: String -> Int -> String -> (Token, Int, String)
      getString acc n s
        | not (head s `elem` "\n\\\"") = getString (head s : acc) (n + 1) (tail s)
        | s =~ "^\""                   = (AString $ reverse acc, n + 1, tail s)
        | s =~ "^\\\\[nt\\\\\"]"       = getString ((case s !! 1 of
                                                       'n'  -> '\n'
                                                       't'  -> '\t'
                                                       '\\' -> '\\'
                                                       '"'  -> '"') : acc) (n + 2) (drop 2 s)
        | s =~ "^\\\\."                = lError (col + n + 1) line path ("Illegal escape character in string : " ++ [s !! 1])
        | s =~ "^\\\\?$"               = lError (col + n) line path "File ended in string literal"
        | s =~ "^\\x0A"                = lError (col + n) line path "Literal newlines not allowed in string literals. Try \\n instead"

lexline :: String -> [Indent]
lexline s = let (_, Indent y) = doIt $ leX ("$line$", s)
            in [y]
