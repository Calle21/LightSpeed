module Nova.Parse.Enum (parseEnum) where

parseEnum :: SpecialParse
parseEnum (m:ms, path, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` one isType >=> one isEqual >=> listof1 (Punct ',') isEnd isTags
     then let (name,_:xs') = isEqual `break` xs
              name'        = gets `map` name
              desc         = trep `map` xs'
          in (m {types = insert name' desc $ types m} : ms,
              path,
              ys)
     else pError ln filename "Couldn't parse enum declaration"
