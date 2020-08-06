module Share where

 -- Types

data Inf = Inf Char Int
         deriving Show

data Use = Use [Library] [(String, String)]
         deriving Show

data Library = Library {libNm    :: String
                       ,libTps   :: [(String, Token)]
                       ,libInfs  :: [(String, Inf)]
                       ,libBinds :: [(String,Token)]}
             deriving Show
