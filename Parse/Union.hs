module CLang.Parse.Union (parseUnion) where

import CLang.Parse.Util
import CLang.Types

parseUnion :: SpecialParse
parseUnion (m:ms, path, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` aTypeName >=> one isEqual >=> listof2 (Punct ',') isEnd aType
     then let (name, _:desc) = isEqual `break` xs
              name'          = gets `map` name
              typed          = trep `map` desc
          in (m {types = insert name' typed (types m)} : ms, ys)
     else pError ln filename "Couldn't parse union declaration"
