module Type.Tok (Tok
                      , colOfTok
                      , tokOfTok
                      , tokChar
                      , tokFloat
                      , tokInt
                      , tokString) where

import Type.Token

type Tok = (Int, Token)

colOfTok = fst
tokOfTok = snd

tokChar :: Tok -> Char
tokChar (_, t) = tokenChar t

tokFloat :: Tok -> Float
tokFloat (_, t) = tokenFloat t

tokInt :: Tok -> Int
tokInt (_, t) = tokenInt t

tokString :: Tok -> String
tokString (_, t) = tokenString t
