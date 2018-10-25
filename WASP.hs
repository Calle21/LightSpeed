module WASP (WASP) where

import Ubi
import Lex
import Indent
import Parse
import Compile

WASP :: IO ()
WASP = do args    <- getArgs
          current <- getCurrentDirectory
          files   <- getFiles current
          let lexed              = mapDir []    lex     files
              indented           = mapDir []    indent  lexed
              (setup, indented') = getSetup indented
              parsed             = mapDir setup  parse   indented
              setup'             = deriveReturns setup parsed
              compiled           = mapDir setup' compile parsed
          writeComp compiled
          if "lib" `elem` args
          then writeLib =<< makeLib current
          else return ()
