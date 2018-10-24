module Type.SpecialParse (SpecialParse) where

import Type.Setup
import Type.Indent

type SpecialParse = (Setup, FilePath, [Indent]) -> (Setup, [Indent])
