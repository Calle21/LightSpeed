module Type.Indent (Indent(..),
                          lineOfIndent,
                          colOfIndent) where

import Type.Tok

data Indent = Line Int [Tok]
            | Indent [Indent]
            deriving (Read, Show)

lineOfIndent, colOfIndent :: Indent -> Int

lineOfIndent   (Line ln _)    = ln
lineOfIndent   (Indent (y:_)) = lineOfIndent y

colOfIndent (Line _ (x:_)) = fst x
colOfIndent (Indent (y:_)) = colOfIndent y
