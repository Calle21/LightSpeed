module Nova.Parse.InsertType (insertType) where

import Nova.Ubi

insertType :: Type -> Setup -> Int -> FilePath -> Setup
insertType t setup@(m:ms) ln path = case checkType setup t of
                                      Nothing -> m {types = Set.insert t $ types m} : ms
                                      Just s  -> pError ln path ("Bad Type : " ++ s)
