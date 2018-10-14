module Nova.Parse.InsertType (insertType) where

insertType :: Setup -> Type -> Setup
insertType (m:ms) (tname,tvars,tdesc) = m {types = insert t $ types m} : ms
