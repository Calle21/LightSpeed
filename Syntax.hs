module Syntax where

import qualified Data.ByteString.Char8 as C
import Ubi
import Util

annotation s = s `arrayElem` annotations

annotations = packit ["mutable",
                      "static"]

charword s' = isJust (s' `lookup` charwords)

charwords = packassoc [("nul"  ,   '\NUL')
                     , ("tab"  ,   '\t')
                     , ("newline", '\n')
                     , ("space",   ' ')]

charwordChar s' = fromJust $ (s' `lookup` charwords)

isPathNova p = C.pack (takeExtension p) == C.pack ".nova"

isPathUnnova p = C.pack (takeFileName p) `arrayElem` unnovas

punctuation c = c `C.elem` punctuations

punctuations = C.pack "()[]{}.,@#$"

reserved s = s `arrayElem` reserveds

reserveds = packit ["->",
                    "...",
                    "::",
                    ":=",
                    "<-",
                    "=",
                    "?",
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
                    "syscall",
                    "tag",
                    "then",
                    "throw",
                    "type",
                    "typecase",
                    "union",
                    "where",
                    "yes"]

specialChar c = c `C.elem` specialChars

specialChars = C.pack "\\!%&*+-/:;<=>?^|~"

specialFile path = C.pack (takeBaseName path) `arrayElem` specialFiles 

specialFiles = packit ["autotag",
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

symChar c = c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c >= '0' && c <= '9' ||
            c `C.elem` (C.pack "_`'" `C.append` specialChars)

synFloat      s' = C.unpack s' =~ "^-?\\d+\\.\\d+$" :: Bool

synHexChar    s' = C.unpack s' =~ "^[0-9a-fA-F]{2}$" :: Bool

synInt        s' = C.unpack s' =~ "^-?\\d+$" :: Bool

synKeyword    s' = let s = C.unpack s'
                   in s =~ "^[a-z0-9]*'?$" && any isLower s

synOpname     s' = C.unpack s' =~ ("^[\\\\" ++ C.unpack specialChars ++ "]+$") :: Bool

synOpnameText s' = C.unpack s' =~ "^`[a-z]{2..}`$" :: Bool

synOption     s' = C.unpack s' =~ "^%[a-z]{2..}%$" :: Bool

synSpecial    s' = C.unpack s' =~ "^\*[a-zA-Z0-9]+\*$" :: Bool

synType       s' = let s = C.unpack s'
                   in s =~ "^[A-Z][a-zA-Z0-9]*$" && not (all isUpper s)

synVartype    s' = C.unpack s' =~ "^[A-Z]+$" :: Bool

unnovas = packit [".use",".chain",".export",".exe"]
