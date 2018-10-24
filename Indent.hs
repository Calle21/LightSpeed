module Indent (indent) where
 
import Error
import Type.CompFN
import Type.File(File(Indented, Lexed))
import Type.Indent
import Type.Lex
import Type.Tok
import Type.Token(Token(Reserved, Punctuation))

indent :: CompFN
indent _ (_, Lexed xs) = Indented $ fst $ getIndent [] 1 xs
  where
  getIndent :: [Indent] -> Int -> [Lex] -> (Indent, [Lex])
  getIndent acc level toks@(x:_) =
    case compare (colOfLex x) level of
      LT -> (Indent $ reverse acc, toks)
      EQ -> let (y,toks') = aline [] (lineOfLex x) 0 toks
            in getIndent (y : acc) level toks'
      GT -> let (y,toks') = getIndent [] (colOfLex x) toks
                acc'      = y : acc
            in if null toks' || colOfLex (head toks') < level
               then (Indent $ reverse acc', toks')
               else getIndent acc' level toks'
  getIndent acc _ _ = (Indent $ reverse acc, [])

aline :: [Tok] -> Int -> Int -> [Lex] -> (Indent, [Lex])
aline acc ln backs (x:xs)
  | lineOfLex x == ln = if tokOfLex x == Reserved "\\"
                        then aline ((colOfLex x, Punctuation '(') : acc) ln (backs + 1) xs
                        else aline ((colOfLex x, tokOfLex x)      : acc) ln  backs      xs
aline acc ln backs xs = (Line ln $ npunct backs acc, xs)
  where
  npunct :: Int -> [Tok] -> [Tok]
  npunct 0 toks = reverse toks
  npunct n toks = let c = colOfTok $ head toks
                  in npunct (n - 1) $ (c + 1, Punctuation ')') : toks
