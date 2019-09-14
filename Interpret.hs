module Interpret () where

 -- Special functions:
 -- +, -, <, >

data Environment = Environment {global :: [Map String ]
                              , local  :: [(String,)]}
                 deriving (Read, Show)

interpret :: Environment -> Indent -> Token
interpret env 
