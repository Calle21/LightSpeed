module CLang.Mods (Module(..), getMod, empty) where

import qualified Data.Map as Map
import CLang.Parse(Code)
import CLang.Types
import Data.Map(Map)
import System.Directory(listDirectory, makeAbsolute)

modules :: IO [(String, Module)]
modules = do
  names <- listDirectory ".modules"
  paths <- makeAbsolute `mapM` names
  ss    <- readFile `mapM` paths
  mods  <- read `mapM` ss
  return (names `zip` mods)

getMod :: String -> IO Environment
getMod s = do mods <- modules
              case s `lookup` mods of
                Just mod -> return mod
                Nothing  -> error ("Couldn't find module : " ++ s)

empty :: Module
empty = Module Map.empty Map.empty Map.empty
