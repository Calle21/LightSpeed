module Nova.Parse (parse) where

import Nova.Parse.File
import Nova.Parse.Specials
import Nova.Ubi

parse :: [(FilePath, Indent)] -> IO [(String, Module)]
parse files = do
  let (specfiles,novafiles)  = partition (\(path,_) -> path `elem` specialFiles) files
  let ([usefile],specfiles') = partition (\(path,_) -> path == "use")            specfiles
  setup <- parseUse usefile
  let setup' = foldl parseSpecialFile [] $ sortBy (comparing fst) specfiles' -- puts "autotag", "chain" and "ops" before "tag"
  return $ foldl parseFile setup' novafiles

parseSpecialFile :: Setup -> (FilePath, Indent) -> Setup
parseSpecialFile setup (path,Indent ys) = case path of -- Compiler sorts out other entries
                                            "autotag" -> foldit setup parseAutotag ys
                                            "chain"   -> foldit setup parseChain   ys
                                            "enum"    -> foldit setup parseEnum    ys
                                            "ops"     -> foldit setup parseOps     ys
                                            "struct"  -> foldit setup parseStruct  ys
                                            "synonym" -> foldit setup parseSynonum ys
                                            "tag"     -> foldit setup parseTag     ys
                                            "type"    -> foldit setup parseType    ys
                                            "union"   -> foldit setup parseUnion   ys
  where
  foldit :: Setup -> SpecialParse -> [Indent] -> Setup
  foldit setup _    [] = setup
  foldit setup prse ys = let (setup', ys') = prse (setup, path, ys)
                         in foldit setup' prse ys'

