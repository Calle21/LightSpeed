module Nova.Parse.Struct (parseStruct) where

import Nova.Types
import Nova.Parse.Util

parseStruct :: SpecialParse
parseStruct (setup, path, y:ys) =
  let (ln:xs) = theLine y path
  in if xs `match` aTypeName >=> isEnd
     then let tname         = gets `map` xs
              (fields, ys') = ys `follows` 4
              unfields      | null fields = pError (ln + 1) path "No fields given for struct definition"
                            | otherwise   = readField `map` fields
                where
                readField :: Indent -> ([String], String)
                readField (Line ln xs) = if xs `match` aType >=> one isName >=> isEnd
                                         then (gets `map` init xs, gets $ last xs)
                                         else pError ln path "Bad struct field"
              accessfn      = makeFn `mapWI` unfields
                where
                makeFn :: Int -> ([String], String) -> Binding
                makeFn i (fieldtyp, fieldname) = Binding {locals  = [],
                                                          pattern = (fieldtyp, [PatK fieldname, PatT tname]
                                                          params  = ["arg"],
                                                          value   = parseBody 1 (m:ms) $ lexline $ format "lw (arg + `0`)" [i]
              typed         = toLower `map` head tname : "(" : "," `intersperse` (fst `map` unfields) ++ [")"]
          in (m {types    = insert tname typed $ types m,
                 bindings = accessfn ++ bindings m} : ms, ys')
     else pError ln path "Bad struct name (it should be a type followed by one or zero vartype)"
