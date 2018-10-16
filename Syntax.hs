module Nova.Syntax (reserved, specialChars, symChar) where

reserved s = s `elem` ["->",
                       "...",
                       "::",
                       ":=",
                       "<-",
                       "=",
                       "_",
                       "catch",
                       "else",
                       "enum",
                       "go",
                       "has",
                       "if",
                       "infixl",
                       "infixr",
                       "intermediate",
                       "is",
                       "let",
                       "the",
                       "mkarray",
                       "module",
                       "mutable",
                       "no",
                       "postfix",
                       "prefix",
                       "scope",
                       "sizeof",
                       "static",
                       "struct",
                       "switch",
                       "synonym",
                       "tag",
                       "throw",
                       "type",
                       "typecase",
                       "union",
                       "where",
                       "yes"]

specialChars = "\\!$%&*+-/:;<=>?^|~"

symChar :: Char -> Bool
symChar c = c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c >= '0' && c <= '9' ||
            c `elem` "_#`'" ++ specialChars
