
if tail xs `match` one isType >=> oneOrNone isVartype >=> one (is Keyword "=") >=> oneOrMore isTypeLang
then let (thetype, _:desc) = (is $ Keyword "=") `break` (tail xs)
     in 
else pError ln filename "Couldn't parse type declaration"
  replace :: LTok -> String
  replace (_, Name s)    = s
  replace (_, Punct _)   = "|"
  replace (_, Type s)    = s
  replace (_, Vartype s) = s
  updateType :: String -> [String] -> Setup
  updateType name desc =
    let (e:es) = setup
        newt   = insert name desc (types e)
        newf   = enumFNs . records . constructors $ fns e
          where
          enumFNs, records, constructors :: FNMap -> FNMap
          constructors = id
          records      = id
          enumFNs t | enum desc = foldlWI makeEnum t (everyOther desc)
                    | otherwise = t
            where
            enum :: [String] -> Bool
            enum desc = listof1' "|" names
            makeEnum :: Map String Code -> String -> Map String Code
            makeEnum t i name = insert string {- Int name = i -} t
    in e {types = newt, fns = newf} : es

