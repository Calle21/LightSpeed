module Indent where
 
import qualified Data.ByteString.Char8 as C
import Types

indent :: CompFN
indent _ (Lexed xs) = Indented $ fst $ getLines [] 1 xs
  where
  getLines :: [Line] -> Int -> [Tok2] -> (Line, [Tok2])
  getLines acc level toks@(x:_) =
    case compare (colTok2 x) level of
      LT -> (Fold $ reverse acc, toks)
      EQ -> let (y,toks') = aline [] (lineTok2 x) 0 toks
            in getLines (y : acc) level toks'
      GT -> let (y,toks') = getLines [] (colTok2 x) toks
                acc'      = y : acc
            in if null toks' || colTok2 (head toks') < level
               then (Fold $ reverse acc', toks')
               else getLines acc' level toks'
    where
    aline :: [Tok1] -> Int -> Int -> [Tok2] -> (Line, [Tok2])
    aline acc ln backs (x:xs)
      | lineTok2 x == ln = if tokTok2 x == Reserved (C.singleton '\\')
                           then aline ((colTok2 x, Punctuation '(') : acc) ln (backs + 1) xs
                           else aline ((colTok2 x, tokTok2 x)       : acc) ln  backs      xs
    aline acc ln backs xs = (Line ln $ npunct backs acc, xs)
      where
      npunct :: Int -> [Tok1] -> [Tok1]
      npunct 0 toks = reverse toks
      npunct n toks = let c = colTok1 $ head toks
                      in npunct (n - 1) $ (c + 1, Punctuation ')') : toks
  getLines acc _ _ = (Fold $ reverse acc, [])
