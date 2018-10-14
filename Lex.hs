module Nova.Lex (lex, lexline) where

import Nova.Indent
import Nova.Syntax
import Nova.Ubi

lex :: [FilePath] -> IO [(FilePath, [Lex])]
lex paths = do
  ss <- readFile `mapM` paths
  return $ leX `map` (paths `zip` ss)

leX :: (FilePath, String) -> (FilePath, [Lex])
leX (path, s) = (path, loop 1 1 s)
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
    token c = if c `elem` ".,()[]{}" then (Punct c, 1, tail inp)
              else case c of
                '"'  -> getString "" 1 (tail inp)
                '\'' -> let (p,n) = inp =~ "^'([^\\\\']|(\\\\([nt\\\\']|x[0-9a-fA-F]{2})))'" :: (Int,Int)
                        in if p == (-1)
                           then lError col line path "Bad character token"
                           else ((AChar $ read $ take n inp), n, drop n inp)
                _    -> let (s, inp')                                      = span symChar inp
                            sym | null s                                   = lError col line path ("Illegal character : " ++ [c])
                                | reserved s                               = Reserved s
                                | names s                                  = Name s
                                | types s                                  = Type s
                                | s =~ ("^[\\\\" ++ specialChars ++ "]+$") = Opname s
                                | s =~ "^-?\\d+$"                          = AInt $ read s
                                | s =~ "^-?\\d+\\.\\d+$"                   = AFloat $ read s
                                | s =~ "^#\\d+$"                           = HashI $ read $ tail s
                                | head s == '#' && names (tail s)          = HashBind $ tail s
                                | s =~ "^\\*[A-Z0-9]+\\*$"                 = Special s
                                | s =~ "^[A-Z]+$"                          = Vartype s
                                | s =~ "^[a-zA-Z_][a-zA-Z0-9_]*:$"         = Tag $ init s
                                | otherwise                                = lError col line path ("Bad token : " ++ s)
                        in (sym, length s, inp')
      where
      names, types :: String -> Bool
      names s = s =~ "^[a-z0-9]*$" && any isLower s
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
