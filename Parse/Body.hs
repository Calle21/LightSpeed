module CLang.Parse.Body (parseBody) where

data ParenType = TPLE Bool
               | TYPE Bool
               | RECC Bool
               | CBNM Bool

parseBody :: Setup -> (String, Indent) -> Setup
parseBody setup (filename, Indent ys) = loop setup ys
  where
  loop :: Setup -> [Indent] -> Setup
  loop setup (y:ys) =
    let (ln, xs) = theLine y filename
    in case fdecl of
         Just xs' -> let (rettype, name, ptype) = getfdecl xs
                         (whereexpr, (exprln, ys'))
                           | xs' `match` end = ([], ys `follows` 2)
                           | xs' `match` one (is (Keyword "where")) >=> notEnd =
                               let colNr      = fst . head . tail $ xs'
                                   (lns, ys') = (Line ln $ tail xs', ys) `followed` colNr
                               in (lns, ys' `follows` 2)
                           | xs' `match` anequals >=> notEnd =
                               let colNr = fst . head . tail $ xs'
                               in ([], (Line ln $ tail xs', ys) `followed` colNr
                     in 
         Nothing  -> pError ln filename "Couldn't parse function declaration"
  loop setup _      = setup

