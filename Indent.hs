module Nova.Indent (indent) where

import Nova.Type
import Nova.Error

indent :: CompFN
indent (_, (path, Lexed xs)) = Indented $ fst $ getIndent [] 1 xs
  where
  getIndent :: [Indent] -> Int -> [Lex] -> (Indent, [Lex])
  getIndent acc level toks@(x:_) =
    case compare (getcl x) level of
      LT -> make acc toks
      EQ -> let (y,toks') = iline [] (getln x) 0 toks
            in getIndent (y : acc) level toks'
      GT -> let (y,toks') = getIndent [] (getcl x) toks
                acc'      = y : acc
            in if null toks' || getcl (head toks') < level
               then make acc' toks'
               else getIndent acc' level toks'
  getIndent acc _     _ = make acc []
  make :: [Indent] -> [Lex] -> (Indent, [Lex])
  make acc toks = (Indent $ reverse acc, toks)

iline :: [Tok] -> Int -> Int -> [Lex] -> (Indent, [Lex])
iline acc ln backs (x:xs)
  | getln x == ln = if gettk x == Reserved "\\"
                    then iline ((getcl x, Punct '(') : acc) ln (backs + 1) xs
                    else iline ((getcl x, gettk x) : acc)   ln  backs      xs
iline acc ln backs xs = (Line ln $ reverse (npunct backs acc), xs)
  where
  npunct :: Int -> [Tok] -> [Tok]
  npunct 0 toks = toks
  npunct n ts = let c = getColumn $ head ts
                in npunct (n - 1) $ (ln, c + 1, Punct ')') : toks
