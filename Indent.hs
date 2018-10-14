module CLang.Indent (indent, doIt) where

import CLang.Types
import CLang.Error

indent :: [(FilePath, [Lex])] -> [(FilePath, Indent)]
indent files = doIt `map` files

doIt :: (FilePath, [Lex]) -> (FilePath, Indent)
doIt (path, xs) = (path, fst $ getIndent [] 1 xs)
  where
  getIndent :: [Indent] -> Int -> [Lex] -> (Indent, [Lex])
  getIndent acc level toks@(x:_) =
    case level `compare` getcl x of
      GT  -> make (acc, toks)
      EQ  -> let (y,toks') = getLine [] (getln x) 0 toks
             in getIndent (y : acc) level toks'
      LT  -> let (y,toks') = getIndent [] (getcl x) toks
                 acc'      = y : acc
             in if null toks' || getcl (head toks') < level
                then make (acc', toks')
                else getIndent acc' level toks'
  getIndent acc _     _ = make (acc, [])
  make :: ([Indent], [Lex]) -> (Indent, [Lex])
  make (acc,toks) = (Indent $ reverse acc, toks)
  getLine :: [Tok] -> Int -> Int -> [Lex] -> (Indent, [Lex])
  getLine acc ln backs (x:xs)
    | getln x == ln = if gettk x == Reserved "\\"
                       then getLine ((getcl x, Punct '(') : acc) ln (backs + 1) xs
                       else getLine ((getcl x, gettk x) : acc)   ln  backs      xs
  getLine acc ln backs xs = (Line ln $ reverse (npunct backs acc), xs)
    where
    npunct :: Int -> [Tok] -> [Tok]
    npunct 0 toks = toks
    npunct n ts = let c = getColumn $ head ts
                  in npunct (n - 1) $ (ln, c + 1, Punct ')') : toks
