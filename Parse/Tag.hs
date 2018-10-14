
if tagsyntax (tail xs)
then let name = snd . head $ tail xs
         (param,xs') = getList (Punct ',') isEndParen (drop 2 xs)
         (lns,ys')   = case xs' of
                         (_,Keyword "="):xs'' -> (Line ln xs'', ys) `followed` getCol xs''
                         []                   -> ys `follows` (getCol (tail xs) + 2)
     if null lns then pError (ln + 1) filename "Empty body of tag function"
     else undefined
tagsyntax :: [LTok] -> Bool
tagsyntax xs = xs `match` one isType >=> one isStartParen >=> aType >=> one isName >=> one isEndParen >=> isEnd `or` one (is (Keyword "="))
