module Nova.Error (cError) where

format :: String -> [String] -> String
format s ss = let numArg = length ss
              in bind numArg s
  where
  bind numArg = loop
    where
    loop (c:cs)
      | c == '%'  = case cs of
                      []        -> error "Format string ended with %"
                      ('%':cs') -> '%' : loop cs'
                      (d:cs')   ->
                        let argnum | not (isDigit d) = error "Format expected digit or % after %"
                                   | otherwise       = digitToInt d
                            argstr | argnum > numArg = error "Not that many arguments"
                                   | otherwise       = ss !! argnum
                        in argstr ++ loop cs'
      | otherwise = c : loop cs
    loop [] = ""

lError :: Int -> Int -> String -> String -> a
lError (col,line,filename,message) = error $ format "Lex error on line %0, column %1\n  %2 (%3)\n" [show line, show col, message, filename]

pError :: Int -> String -> String -> a
pError line filename message = error $ format "Parse error on line %0\n  %1 (%2)\n" [show line, message, filename]
