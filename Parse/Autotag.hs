module Nova.Parse.Autotag (parseAutotag) where

parseAutotag :: SpecialParse
parseAutotag (m:ms, path, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` one isType >=> one isName >=> one isEqual >=> one isType >=> isEnd
     then let (name,_:tag) = isEqual `break` xs
              name'        = gets `map` name
              tag'         = gets $ head tag
          in (m {autotags = (name,tag) : autotags m} : ms, ys)
     else pError ln filename "Couldn't parse autotag"

