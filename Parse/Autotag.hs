module Nova.Parse.Autotag (parseAutotag) where

parseAutotag :: SpecialParse
parseAutotag (m:ms, path, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` one isType >=> one isName >=> one isEqual >=> one isType >=> isEnd
     then (m {autotags = (gets `map` take 2 xs, gets $ xs !! 3) : autotags m} : ms, ys)
     else pError ln filename "Couldn't parse autotag"

