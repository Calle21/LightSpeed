module Error (lError, pError) where

import Util (format)

lError :: Int -> Int -> String -> String -> a
lError col line filename message = error $ format "Lex error on line %0, column %1\n  %2 (%3)\n" [show line, show col, message, filename]

pError :: Int -> String -> String -> a
pError line filename message = error $ format "Parse error on line %0\n  %1 (%2)\n" [show line, message, filename]
