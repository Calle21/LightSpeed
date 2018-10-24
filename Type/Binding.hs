module Type.Binding (Binding(..)) where

data Binding = Binding {locals  :: [Binding],
                        bindpat :: Pattern,
                        params  :: BindPat,
                        value   :: Code}
              deriving (Read, Show)
