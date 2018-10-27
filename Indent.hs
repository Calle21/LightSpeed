module Indent where
 
import qualified Data.ByteString.Char8 as C
import Types

indent :: CompFN
indent (Lexed xs) = Indented $ fst $ getIndent [] 1 xs
  where
  getIndent :: [Indent] -> Int -> [Tok2] -> (Indent, [Tok2])
  getIndent acc level toks@(x:_) =
    case compare (tok2Col x) level of
      LT -> (Indent $ reverse acc, toks)
      EQ -> let (y,toks') = aline [] (tok2Line x) 0 toks
            in getIndent (y : acc) level toks'
      GT -> let (y,toks') = getIndent [] (tok2Col x) toks
                acc'      = y : acc
            in if null toks' || tok2Col (head toks') < level
               then (Indent $ reverse acc', toks')
               else getIndent acc' level toks'
  getIndent acc _ _ = (Indent $ reverse acc, [])

aline :: [Tok1] -> Int -> Int -> [Tok2] -> (Indent, [Tok2])
aline acc ln backs (x:xs)
  | tok2Line x == ln = if tok2Tok x == Reserved (C.singleton '\\')
                       then aline ((tok2Col x, Punctuation '(') : acc) ln (backs + 1) xs
                       else aline ((tok2Col x, tok2Tok x)      : acc) ln  backs      xs
aline acc ln backs xs = (Line ln $ npunct backs acc, xs)
  where
  npunct :: Int -> [Tok1] -> [Tok1]
  npunct 0 toks = reverse toks
  npunct n toks = let c = tok1Col $ head toks
                  in npunct (n - 1) $ (c + 1, Punctuation ')') : toks
