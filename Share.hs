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

data Token = Access [String]
           | AllCap String
           | Array [Int] [Token]
           | AR Int Token
           | As String Token
           | Cap String
           | CPat [String]
           | Destroy Token [(Token,Token)]
           | DR
           | EOF
           | From String Token
           | Funcall Token Token
           | Function Token [Token] Token
           | FN Token Token
           | Hash Int
           | If Token Token Token
           | IN
           | Int Int64
           | Keyword String
           | Let [(Token,String,Token)] Token
           | Loop String
           | LoopParam Token String Token
           | Modifier Token Token
           | Modify Token [(Token,Token)]
           | Name String
           | NM String [Token]
           | Op String
           | Param Token String
           | Part Int
           | PR [String] Token
           | Punct Char
           | Pure Token
           | Seq [Token]
           | SetCol Int
           | SetFileName String
           | SetLine Int
           | ST String [(Token,String)]
           | Struct String [Token]
           | Switch Token [Token]
           | TCase Token [(Token,Token)] Token
           | The Token Token
           | TNP Token String [Token]
           | Tuple [Token]
           | TU [Token]
           | TP String Token
           | Type String Token
           | TypeAndName Token String
           | Typed Token Token
           | UN [Token]
           deriving (Eq, Show)
