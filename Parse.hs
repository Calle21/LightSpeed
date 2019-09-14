module Parse where

import Parse.File
import Parse.Specials
import Parse.Use
import Ubi

parse :: [(FilePath,CompFile)] -> [(FilePath,CompFile)]
parse files = case find isUseFile files of
                Nothing    -> error "Couldn't find usefile :("
                Just (_,i) -> let table = parseUseFile i
                              in sweep (parseFile table) (remove isUseFile files)
  where
  isUseFile (p,_) = takeFileName p == "use"
  
