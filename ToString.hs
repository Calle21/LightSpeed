module CLang.ToString where

import CLang.Lex(Token(..))
import CLang.Indent(Indent(..), getColumn)
import Data.List(intercalate)

tokenString :: Token -> String
tokenString t = case t of
                  Hash n      -> '#' : show n
                  Infix s     -> '`' : s ++ "`"
                  InfixDecl e -> case e of
                                   Right p  -> 'R' : show p
                                   Left  p  -> 'L' : show p
                  Keyword s   -> s
                  Name s      -> s
                  Nested ss   -> "." `intercalate` ss
                  Opname s    -> s
                  Punct c     -> [c]
                  Special s   -> s
                  TChar c     -> show c
                  TFloat f    -> show f
                  TInt n      -> show n
                  TString s   -> show s
                  Type s      -> s
                  Typevar s   -> s
                  Underscore  -> "_"

indentString :: Indent -> String
indentString (Line _ xs) = pred (fst $ head xs) `replicate` ' ' ++ " " `intercalate` ((tokenString . snd) `map` xs) ++ "\n"
indentString (Indent ys) | getColumn ys == 1 = separate $ indentString `map` ys
  where                  | otherwise         = indentString `concatMap` is
  separate :: [String] -> String
  separate (x:y:xs) = x ++ (if head y /= ' ' then "\n" else "") ++ separate (y:xs)
  separate (x:_)    = x
