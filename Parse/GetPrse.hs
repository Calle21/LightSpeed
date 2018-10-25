module Parse.GetSetup (getSetup) where

import Type.DirFile(Directory, 
import Ubi
import Util (listDirectory', (|||))

getSetup :: Directory -> (Setup, Directory)
getSetup dir = let setup = case find isUse dir of
                             Just (_,y) -> parseUse y
                             Nothing    -> error "Couldn't find use-file"
                   setup' = case find isChain dir of
                              Just (_,y) -> parseChain setup y
                              Nothing    -> setup
                   setup'' = case find isOps dir of
                               Just (_,dir') -> parseOps dir'
                               Nothing       -> setup'
                   (setup'', filter (not . (isUse ||| isChain ||| isOps)) dir)
  where
  isChain, isUse, isOps :: DirFile -> Bool
  isUse dirf = case dirf of
                 File (p,_) -> takeFileName p == ".use"
                 _          -> False
  isChain dirf = case dirf of
                   File (p,_) -> takeFileName == ".chain"
                   _          -> False
  isOps dirf = case dirf of
                 Folder (p,_) -> takeFileName p == "operators"
                 _            -> False
