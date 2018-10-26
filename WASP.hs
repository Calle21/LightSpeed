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
          files   <- getFiles current
          let lexed                        = mapFolder []              novalex files
              indented                     = mapFolder []              indent  lexed
              (parserSpecifics, indented') =        getParserSpecifics         indented
              parsed                       = mapFolder parserSpecifics parse   indented
              protos                       = getProtos parserSpecifics         parsed
              compiled                     = mapFolder protos          compile parsed
          writeComp compiled
          if "lib" `elem` args
          then writeLib =<< makeLib current
          else return ()
