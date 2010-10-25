{-# LANGUAGE PackageImports #-}

-- Configuration for docidx.

module Distribution.DocIdx.Config where

import Control.Monad
import "monads-tf" Control.Monad.Writer
import Data.Maybe
import System.Directory
import System.FilePath

import Distribution.DocIdx.Common

-- | Name of configuration file within user's application data
-- directory.
cfgFile :: FilePath
cfgFile = "config"

-- | Configuration information for docidx; at present only the
-- tocExtras part is exposed via the config file, however.
data DocIdxCfg = DocIdxCfg {
    pageTitle :: String
  , pageCss :: [String]
  , favIcon :: String
  , tocExtras :: [TocItem]
  } deriving Show

-- | Default configuration.
defaultConfig :: DocIdxCfg
defaultConfig = DocIdxCfg {
    pageTitle = "Local Haskell package docs"
  , pageCss = ["http://hackage.haskell.org/packages/hackage.css"]
  , favIcon = "http://hackage.haskell.org/images/Cabal-tiny.png"
  , tocExtras = []
  }

-- | Read configuration file if present.
getConfig :: IO DocIdxCfg
getConfig = do
  appDir <- getAppUserDataDirectory appName
  let cfgPath = joinPath [appDir, cfgFile]
  there <- doesFileExist cfgPath
  extras <- if not there
              then return []
              else do dm <- tryReadFile cfgPath
                      case dm of
                        Nothing -> return []
                        Just d -> readConfig d
  return $ defaultConfig { tocExtras = extras }

-- | Read a config file's contents.  At present we're only looking for
-- TocItems, but other things could be there in the future
-- (e.g. alternative CSS, etc.)
readConfig :: String -> IO [TocItem]
readConfig d = do
  let (extras, l) = runWriter $ (liftM catMaybes . mapM readConfigLine) $ lines d
  forM_ l putStrLn
  return extras

-- | Try to read a single line from the config file.
readConfigLine :: String -> Writer [String] (Maybe TocItem)
readConfigLine line = do
  let ws = words line
  case ws of
    [] -> return Nothing
    ("--":_) -> return Nothing
    ("extraSeparator":_) -> return $ Just TocSeparator
    ("extraNewline":_) -> return $ Just TocNewline
    ("extra":xs) -> if length xs > 1
                      then let name = unlines $ init xs
                               url = last xs
                           in return $ Just $ TocItem name url
                      else warn "malformed extra" line
    _ -> warn "unrecognised config line" line

-- | Moan gently about any weird looking lines.
warn :: String -> String -> Writer [String] (Maybe a)
warn msg line = do tell ["Warning: " ++ msg ++ ": \"" ++ line ++ "\""]
                   return Nothing
