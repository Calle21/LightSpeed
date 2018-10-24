module Nova.Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Prelude hiding (getLine)





type LChain = [(String, String)]

type LFix = Map String Fixity

type LTag = [([String],String)]

type LTyp = Map [String] [String]

type LVal = Map Identifier [Binding]




