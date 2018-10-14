module Nova.Parse.Ops (parseOps) where

import Nova.Parse.Util
import Nova.Types

parseOps :: SpecialParse
parseOps (m:ms, _, y:ys) =
  let (ln,xs) = theLine y "ops"
  in if xs `match` aFixity >=> oneOrMore isOpname >=> isEnd
     then let (s,xs') = (gets $ head xs, tail xs)
          in case s of
               "infixr"  -> withStrength Infixr xs'
               "infixl"  -> withStrength Infixl xs'
               "prefix"  -> makeIt Prefix  xs'
               "postfix" -> makeIt Postfix xs'
     else pError ln "ops" "Couldn't parse fixity"
  where
  withStrength :: (Int -> Fixity) -> [Tok] -> (Setup, [Indent])
  withStrength mkfn (x:xs) = makeIt xs $ mkfn $ geti $ head xs
  makeIt :: Fixity -> [Tok] -> (Setup, [Indent])
  makeIt fix xs = (foldl (insertIt fix) setup xs, ys)
    where
    insertIt :: Fixity -> Setup -> Tok -> Setup
    insertIt fix (m:ms) (_, t) = m {fixity = insert (getS t) fix $ fixity m} : ms
