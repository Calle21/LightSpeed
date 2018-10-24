module Type.FunctionType (FunctionType(..)) where

data FunctionType = Action | Pure
                  deriving (Eq, Read, Show, Enum)
