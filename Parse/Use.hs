module Parse.Use (parseUse) where

import qualified Mods as Mod
import Mods(getMod)
import Types
import Parse.Util

parseUse :: Indent -> IO Setup
parseUse (Indent ys) | all (`match` one isType) ys = mapM getMod ys
  where
  getMod (Line 1 (_,Type s)) = getMod s
parseUse _ = error "Couldn't parse use file"
