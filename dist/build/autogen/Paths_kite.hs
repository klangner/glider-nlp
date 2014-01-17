module Paths_kite (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/klangner/.cabal/bin"
libdir     = "/home/klangner/.cabal/lib/kite-0.1/ghc-7.6.3"
datadir    = "/home/klangner/.cabal/share/kite-0.1"
libexecdir = "/home/klangner/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "kite_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "kite_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "kite_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "kite_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
