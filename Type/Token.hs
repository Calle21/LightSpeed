module Type.Token (Token(..), tokenChar, tokenFloat, tokenInt, tokenString) where

data Token = Keyword       String
           | Opname        String
           | Option        String
           | Punctuation   Char
           | Reserved      String
           | Special       String
           | TokenChar     Char
           | TokenFloat    Float
           | TokenInt      Int
           | TokenString   String
           | Type          String
           | Vartype       String
           deriving (Eq, Read, Show)

tokenChar :: Token -> Char
tokenChar (Punctuation c) = c
tokenChar (TokenChar c)   = c

tokenFloat :: Token -> Float
tokenFloat (TokenFloat f) = f

tokenInt :: Token -> Int
tokenInt (TokenInt i)  = i

tokenString :: Token -> String
tokenString (Keyword s)     = s
tokenString (Opname s)      = s
tokenString (Reserved s)    = s
tokenString (Special s)     = s
tokenString (TokenString s) = s
tokenString (Type s)        = s
tokenString (Vartype s)     = s
