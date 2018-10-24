module Nova.Libs (getLib, newlib) where


import qualified Data.Map as Map
import System.Directory

getLib :: String -> IO Lib
getLib s = do libs' <- libs
              case s `lookup` libs' of
                Just lib -> return lib
                Nothing  -> error ("Couldn't find lib : " ++ s)

libs :: IO [(String, Lib)]
libs = do
  names <- listDirectory "~/.novalibs"
  paths <- makeAbsolute `mapM` names
  ss    <- readFile `mapM` paths
  libs  <- read `mapM` ss
  return (names `zip` libs)

newlib :: Lib
newlib = undefined
