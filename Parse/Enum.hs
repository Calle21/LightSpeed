module Nova.Parse.Enum (parseEnum) where

parseEnum :: SpecialParse
parseEnum (setup, path, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` one isType >=> one isEqual >=> listof1 (is (Reserved "->")) isEnd isConstr
     then (insertType (Type (gets $ head xs, "", trep `map` drop 2 xs)) setup, ys)
     else pError ln filename "Couldn't parse enum declaration"
