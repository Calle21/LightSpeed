module CLang.Parse.File (parseFile) where

import CLang.Parse.Enum
import CLang.Parse.Function
import CLang.Parse.Struct
import CLang.Parse.Tag
import CLang.Parse.Type
import CLang.Parse.Union
import CLang.Types

parseFile :: Setup -> (String, Indent) -> Setup
parseFile setup (filename, Indent ys) = loop (setup, ys)
  where
  loop :: (Setup, [Indent]) -> Setup
  loop setup ys@((Line _ (x:_)):_) =
    case snd x of
      Keyword "enum"   -> loop $ parseEnum     filename ys 
      Keyword "struct" -> loop $ parseStruct   filename ys
      Keyword "tag"    -> loop $ parseTag      filename ys 
      Keyword "type"   -> loop $ parseType     filename ys 
      Keyword "union"  -> loop $ parseUnion    filename ys
      _                -> loop $ parseFunction filename ys
  loop setup [] = setup
  loop _ (y:_) = pError (getLine y) filename "Unexpected indentation"
