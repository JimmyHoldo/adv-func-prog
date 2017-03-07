module Paths_replay (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jimmy/.cabal/bin"
libdir     = "/home/jimmy/.cabal/lib/x86_64-linux-ghc-7.10.3/replay-0.1.0.0-C2mRRxm1lZz1Z9uHg7XPv5"
datadir    = "/home/jimmy/.cabal/share/x86_64-linux-ghc-7.10.3/replay-0.1.0.0"
libexecdir = "/home/jimmy/.cabal/libexec"
sysconfdir = "/home/jimmy/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "replay_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "replay_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "replay_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "replay_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "replay_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
