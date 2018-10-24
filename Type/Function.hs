module Type.Function (Function) where

import Type.FunctionType
import Type.Pattern
import Type.Code
import Type.Setup

data Function = Function {fntype  :: FunctionType,
                          rettype :: [String],
                          pattern :: [Pattern],
                          body    :: Code,
                          setup   :: Setup}
              deriving (Read, Show)
