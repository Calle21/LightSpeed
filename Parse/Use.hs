module CLang.Parse.Use (parseUse) where

import qualified CLang.Mods as Mod
import CLang.Mods(getMod)
import CLang.Types
import CLang.Parse.Util

parseUse :: Indent -> IO Setup
parseUse (Indent [(Line 1 xs)]) | xs `match` listOf1 (Keyword "->") isEnd (one (isType `or` isVartype)) =
  let ss = getS `map` (fst $ getList1 isEnd xs)
  in do mods' <- getMod `mapM` tail ss
        return ((head ss, Mod.empty) : mods')
parseUse _ = error "Couldn't parse use file"
