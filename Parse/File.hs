module Parse.File (parseFile) where

import qualified Data.ByteString.Char8 as C
import Parse.Function
import Parse.NickName
import Parse.Type
import Types

parseFile :: Setup -> Sweep
parseFile setup path (Indented (Fold ys)) = loop ys
  where
  loop :: [Line] -> [Code]
  loop [] = []
  loop ys = case ys of
              (Fold _):_      -> error "Unexpected indentation (parseFile)"
              (Line 1 xs):ys' -> let (p,ys'') = case one (is (Keyword (C.pack "type"))) xs of
                                                  Just xs' -> parseTypes xs' ys'
                                                  Nothing  -> case one (is (Keyword (C.pack "nick"))) xs of
                                                                Just xs' -> parseNick xs' ys'
                                                                Nothing  -> parseFunction xs ys'
                                 in p : loop ys''
    case  of
      Keyword "type"     -> loop $ parseType     filename ys 
      Keyword "nickname" -> loop $ parseNick     filename ys
      _                  -> loop $ parseFunction filename ys
  loop setup []    = setup
  loop _     (y:_) = pError (getLine y) filename "Unexpected indentation"
