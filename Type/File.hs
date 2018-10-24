module Type.File(File(..), FileWPath) where

import qualified Data.ByteString.Char8 as C
import Type.Lex(Lex)
import Type.Indent(Indent)
import Type.Code
import Type.NIL(NIL)

data File = Undone   C.ByteString
          | Lexed    [Lex]
          | Indented Indent
          | Parsed   Code
          | Compiled NIL
          deriving (Read, Show)

type FileWPath = (FilePath, File)
