module CLang.Parse.Union (parseUnion) where

import CLang.Parse.Util
import CLang.Types

parseUnion :: SpecialParse
parseUnion (setup, path, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` aTypeName >=> one isEqual >=> one isStartParen >=> listof2 (Punct ',') (one isEndParen) aType >=> isEnd
     then let name        = gets $ head xs
              (vars, xs') = if xs !! 1 == (_, Reserved "=")
                            then ("", drop 2 xs)
                            else (gets $ xs !! 1, drop 3 xs)
              desc        = trep `map` xs'
          in (insertType (Type (name, vars, desc)) setup ln path, ys)
     else pError ln path "Couldn't parse union declaration"
