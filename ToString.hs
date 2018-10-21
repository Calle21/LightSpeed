module Nova.ToString where

import Nova.Ubi

tokenString :: Token -> String
tokenString t = case t of
                  Keyword s     -> s
                  Opname s      -> s
                  Option s      -> s
                  Punctuation c -> [c]
                  Reserved s    -> s
                  Special s     -> s
                  TokenChar c   -> show c
                  TokenFloat f  -> show f
                  TokenInt n    -> show n
                  TokenString s -> show s
                  Type s        -> s
                  Vartype s     -> s

indentString :: Indent -> String
indentString (Line _ xs) = pred (fst $ head xs) `replicate` ' ' ++ " " `intercalate` ((tokenString . snd) `map` xs) ++ "\n"
indentString (Indent ys) | getColumn ys == 1 = separate $ indentString `map` ys
  where                  | otherwise         = indentString `concatMap` is
  separate :: [String] -> String
  separate (x:y:xs) = x ++ (if head y /= ' ' then "\n" else "") ++ separate (y:xs)
  separate (x:_)    = x

putIndent :: DirM
putIndent file = case file of
                   Indented y -> putStrLn (indentString y)
