{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.HyprlandWorkspaceHistory
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Reader for the runtime JSON snapshot written by the hypr-workspace-history
-- Hyprland plugin.
module System.Taffybar.Information.HyprlandWorkspaceHistory
  ( HyprlandWorkspaceHistoryMonitor (..),
    HyprlandWorkspaceHistorySnapshot (..),
    workspaceHistoryStateFileName,
    workspaceHistoryStatePath,
    readHyprlandWorkspaceHistorySnapshot,
    workspaceHistoryOrderForMonitor,
    workspaceHistoryOrderForActiveMonitor,
  )
where

import Control.Exception.Enclosed (catchAny)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

newtype HyprlandWorkspaceHistoryMonitor = HyprlandWorkspaceHistoryMonitor
  { hwhMonitorHistory :: [Int]
  }
  deriving (Eq, Show)

instance FromJSON HyprlandWorkspaceHistoryMonitor where
  parseJSON = withObject "HyprlandWorkspaceHistoryMonitor" $ \v ->
    HyprlandWorkspaceHistoryMonitor
      <$> v .: "history"

data HyprlandWorkspaceHistorySnapshot = HyprlandWorkspaceHistorySnapshot
  { hwhVersion :: Int,
    hwhRevision :: Word64,
    hwhActiveMonitor :: Maybe Text,
    hwhActiveWorkspace :: Maybe Int,
    hwhMonitors :: M.Map Text HyprlandWorkspaceHistoryMonitor
  }
  deriving (Eq, Show)

instance FromJSON HyprlandWorkspaceHistorySnapshot where
  parseJSON = withObject "HyprlandWorkspaceHistorySnapshot" $ \v ->
    HyprlandWorkspaceHistorySnapshot
      <$> v .: "version"
      <*> v .: "revision"
      <*> v .:? "active_monitor"
      <*> v .:? "active_workspace"
      <*> v .: "monitors"

workspaceHistoryStateFileName :: FilePath
workspaceHistoryStateFileName = "hyprland-workspace-history.json"

workspaceHistoryStatePath :: IO (Maybe FilePath)
workspaceHistoryStatePath = do
  mRuntimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  pure $ (</> workspaceHistoryStateFileName) <$> mRuntimeDir

readHyprlandWorkspaceHistorySnapshot :: IO (Either String (Maybe HyprlandWorkspaceHistorySnapshot))
readHyprlandWorkspaceHistorySnapshot =
  ( do
      mPath <- workspaceHistoryStatePath
      case mPath of
        Nothing -> pure $ Right Nothing
        Just path -> do
          exists <- doesFileExist path
          if not exists
            then pure $ Right Nothing
            else do
              bytes <- BS.readFile path
              pure $ Just <$> eitherDecodeStrict' bytes
  )
    `catchAny` \err -> pure $ Left (show err)

workspaceHistoryOrderForMonitor ::
  Text ->
  HyprlandWorkspaceHistorySnapshot ->
  Maybe [Int]
workspaceHistoryOrderForMonitor monitor snapshot =
  hwhMonitorHistory <$> M.lookup monitor (hwhMonitors snapshot)

workspaceHistoryOrderForActiveMonitor ::
  HyprlandWorkspaceHistorySnapshot ->
  Maybe [Int]
workspaceHistoryOrderForActiveMonitor snapshot =
  hwhActiveMonitor snapshot >>= (`workspaceHistoryOrderForMonitor` snapshot)
