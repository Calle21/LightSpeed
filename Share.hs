module Share where

 -- Types

data Inf = Inf Char Int
         deriving Show

data Use = Use [Library] [(String, String)]
         deriving Show

data Library = Library {name     :: String
                       ,types    :: [(String, Token)]
                       ,infixs   :: [(String, Inf)]
                       ,bindings :: [(String,Token)]}
             deriving Show

data Token = Access String Token
           | AllCap String
           | Array [Int] [Token]
           | AR Int Token
           | As Token Token
           | Cap String
           | Char Char
           | EOF
           | Float Double
           | From String Token
           | Funcall Token Token
           | Function Token [Token] Token
           | FN Token Token
           | If Token Token Token
           | IN
           | Int Int
           | Keyword String
           | Let [(Token,String,Token)] Token
           | LU String Token
           | LoopParam Token String Token
           | Modifier Token Token
           | Modify Token [(Token,Token)]
           | Name String
           | Newline
           | Op String
           | Param Token String
           | PR Token
           | Punct Char
           | Seq [Token]
           | SetCol Int
           | SetFileName String
           | String String
           | ST [(Token,String)]
           | Switch Token [Token]
           | SY Token
           | TCase Token [(Token,Token)] Token
           | The Token Token
           | TNP Token String [Token]
           | Tuple [Token]
           | TU [Token]
           | TP Token
           | TypeAndName Token String
           | UN [Token]
           deriving Show
