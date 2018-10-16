module Nova.Parse.Chain (parseChain) where

import Nova.Parse.Util
import Nova.Types

parseChain :: SpecialParse
parseChain (m:ms, _, y:ys) =
  let (ln,xs) = theLine y path
  in if xs `match` one isName >=> one isOpname >=> isEnd
     then let [name,op] = gets `map` xs
          in (m {chains = (name,op) : chains m} : ms, ys)
     else pError ln "chain" "Couldn't parse chain"
