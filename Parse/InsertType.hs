module Nova.Parse.InsertType (insertType) where

import Nova.Ubi

insertType :: Setup -> Type -> Int -> FilePath -> Setup
insertType setup@(m:ms) t ln path = case checkType setup t of
                                      Nothing -> m {types = Set.insert t $ types m,
                                                    bindings = binds ++ bindings m} : ms
                                      Just s  -> pError ln path ("Bad Type : " ++ s)
