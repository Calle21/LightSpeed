module Type.Local (Local(..))

import Type.Binding

data Local = Local {bind        :: Binding,
                    annotations :: [String]}
           | Locals [Binding]
           deriving (Read, Show)
