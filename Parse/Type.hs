module Nova.Parse.Type (parseType) where

parseType :: SpecialParse
parseType (setup, path, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` aTypeName >=> one isEqual
     then let name        = gets $ head xs
              (vars, xs') = if xs !! 1 == (_, Reserved "=")
                            then ("", drop 2 xs)
                            else (gets $ xs !! 1, drop 3 xs)
              (desc, ys') = getDesc ln xs'
          in (insertType (Type (name, vars, desc)) setup ln path, ys')
     else pError ln path "Couldn't parse type declaration"
  where
  getDesc :: Int -> [Tok] -> ([String], [Indent])
  getDesc = undefined
