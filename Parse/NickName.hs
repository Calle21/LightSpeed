module Parse.NickName where

parseNick :: [Tok1] -> [Line] -> (Code,[Line])
parseNick xs ys = case one isType xs of
                    Just xs' -> let (_,Type name) = head xs
                                in case one isEqual xs' of
                                     Just xs'' -> (NickName xs'', ys)
                                     Nothing   -> error "Couldn't parse nick (equal)"
                    Nothing  -> error "Couldn't parse nick (name)"
