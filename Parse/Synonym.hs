module CLang.Parse.Synonym (parseSynonym) where

parseSynonym :: Setup -> Indent -> Setup
parseSynonym (m:ms) (Line ln xs) | xs `match` one isType >=> one (is (Keyword "=")) >=> one isType >=> isEnd =
  let [synonym, _, typ] = map gets xs
  in m {types = insert synonym [typ] (types m)} : ms
