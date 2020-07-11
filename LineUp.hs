module LineUp (lineUp) where

lineUp :: [Expr] -> [[Expr]]
lineUp [] = []
lineUp xs = let (_,ln,fn,_) = head xs
                line        = takeWhile (is ln fn) xs
            in line : lineUp (dropWhile (is ln fn))

is :: Int -> FilePath -> Expr -> Bool
is ln fn (_,ln',fn',_) = ln == ln' && fn == fn'
