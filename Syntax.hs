module Syntax (isAnnotation, reserved, specialChars, specialFiles, symChar) where

isAnnotation :: String -> Bool
isAnnotation s = s `elem` ["mutable",
                           "static"]

reserved s = s `elem` ["->",
                       "...",
                       "::",
                       ":=",
                       "<-",
                       "=",
                       "_",
                       "catch",
                       "do",
                       "else",
                       "enum",
                       "Fields:",
                       "Globals:",
                       "has",
                       "if",
                       "infixl",
                       "infixr",
                       "intermediate",
                       "is",
                       "let",
                       "Locals:",
                       "the",
                       "Methods:",
                       "mkarray",
                       "module",
                       "no",
                       "postfix",
                       "prefix",
                       "Requires:",
                       "scope",
                       "sizeof",
                       "static",
                       "struct",
                       "switch",
                       "synonym",
                       "tag",
                       "then",
                       "throw",
                       "type",
                       "typecase",
                       "union",
                       "where",
                       "yes"]

specialChars = "\\!%&*+-/:;<=>?^|~"

specialFiles = ["autotag",
                "chain",
                "enum",
                "ops",
                "struct",
                "synonym",
                "tag",
                "type",
                "union",
                "use",
                "vstruct"]

symChar :: Char -> Bool
symChar c = c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c >= '0' && c <= '9' ||
            c `elem` "_`'" ++ specialChars
