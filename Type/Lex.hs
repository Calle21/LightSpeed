module Type.Lex (Lex,
                       colOfLex,
                       lineOfLex,
                       tokOfLex) where

import Type.Token

type Lex = (Int, Int, Token)

colOfLex, lineOfLex :: Lex -> Int
colOfLex (c, _, _) = c
lineOfLex (_, l, _) = l

tokOfLex :: Lex -> Token
tokOfLex (_, _, t) = t
