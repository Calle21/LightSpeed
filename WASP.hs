module WASP where

import Compile
import Indent
import Lex
import Parse
import Types
import Ubi

WASP :: IO ()
WASP = do args    <- getArgs
          current <- getCurrentDirectory
          undone  <- getFiles current
          let lexed    = mapFolder undone    novalex
              indented = mapFolder lexed     indent
              parsed   = mapFolder indented (parse   (getPSpec  indented))
              compiled = mapFolder parsed   (compile (getProtos parsed))
          writeComp compiled
