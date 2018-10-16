module Nova.Parse.Struct (parseStruct) where

import Nova.Types
import Nova.Parse.Util

parseStruct :: SpecialParse
parseStruct (setup, path, y:ys) =
  let (ln:xs) = theLine y path
  in if xs `match` aTypeName >=> isEnd
     then let (tname,var)   = (gets $ head xs, if null $ tail xs then "" else gets $ xs !! 1)
              (fields, ys') = ys `follows` 4
              unfields      | null fields = pError (ln + 1) path "No fields given for struct definition"
                            | otherwise   = readField `map` fields
                where
                readField :: Indent -> [String]
                readField (Line ln xs) = if xs `match` aType >=> one isName >=> isEnd
                                         then gets `map` xs
                                         else pError ln path "Bad struct field"
              desc          = toLower `map` tname : "(" : [","] `tintercalate` unfields ++ [")"]
          in (insertType setup (tname,var,desc) ln path, ys')
     else pError ln path "Bad struct name (it should be a type followed by one or zero vartype)"
