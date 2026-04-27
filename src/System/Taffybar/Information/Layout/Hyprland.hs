{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Layout.Hyprland
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared Hyprland layout provider using the Hyprland event socket and a
-- broadcast channel + state MVar.
module System.Taffybar.Information.Layout.Hyprland
  ( HyprlandLayoutProviderConfig (..),
    defaultHyprlandLayoutProviderConfig,
    defaultHyprlandLayoutState,
    isRelevantHyprlandLayoutEvent,
    getHyprlandLayoutStateChanAndVar,
    getHyprlandLayoutStateChanAndVarWith,
    getHyprlandLayoutState,
    getHyprlandLayoutStateWith,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault, taffyFork)
import System.Taffybar.Hyprland (getHyprlandClient, getHyprlandEventChan)
import qualified System.Taffybar.Information.Hyprland as Hypr
import qualified System.Taffybar.Information.Hyprland.API as HyprAPI
import qualified System.Taffybar.Information.Hyprland.Types as HyprTypes
import System.Taffybar.Information.Layout.Model

data HyprlandLayoutProviderConfig = HyprlandLayoutProviderConfig
  { layoutSnapshotGetter :: TaffyIO T.Text,
    layoutEventFilter :: T.Text -> Bool
  }

defaultHyprlandLayoutProviderConfig :: HyprlandLayoutProviderConfig
defaultHyprlandLayoutProviderConfig =
  HyprlandLayoutProviderConfig
    { layoutSnapshotGetter = buildHyprlandLayoutSnapshot,
      layoutEventFilter = isRelevantHyprlandLayoutEvent
    }

defaultHyprlandLayoutState :: LayoutSnapshot
defaultHyprlandLayoutState =
  LayoutSnapshot
    { layoutBackend = LayoutBackendHyprland,
      layoutRevision = 0,
      layoutName = ""
    }

newtype HyprlandLayoutStateChanVar
  = HyprlandLayoutStateChanVar
      (TChan LayoutSnapshot, MVar LayoutSnapshot)

wLog :: (MonadIO m) => Priority -> String -> m ()
wLog level message =
  liftIO $ logM "System.Taffybar.Information.Layout.Hyprland" level message

isRelevantHyprlandLayoutEvent :: T.Text -> Bool
isRelevantHyprlandLayoutEvent line =
  let eventName = T.takeWhile (/= '>') line
   in eventName
        `elem` [ "workspace",
                 "workspacev2",
                 "focusedmon",
                 "monitoradded",
                 "monitorremoved",
                 "configreloaded",
                 "taffybar-hyprland-connected"
               ]

getHyprlandLayoutStateChanAndVar ::
  TaffyIO (TChan LayoutSnapshot, MVar LayoutSnapshot)
getHyprlandLayoutStateChanAndVar =
  getHyprlandLayoutStateChanAndVarWith defaultHyprlandLayoutProviderConfig

getHyprlandLayoutStateChanAndVarWith ::
  HyprlandLayoutProviderConfig ->
  TaffyIO (TChan LayoutSnapshot, MVar LayoutSnapshot)
getHyprlandLayoutStateChanAndVarWith cfg = do
  HyprlandLayoutStateChanVar chanAndVar <- getStateDefault $ buildHyprlandLayoutStateChanVar cfg
  pure chanAndVar

getHyprlandLayoutState :: TaffyIO LayoutSnapshot
getHyprlandLayoutState =
  getHyprlandLayoutStateWith defaultHyprlandLayoutProviderConfig

getHyprlandLayoutStateWith :: HyprlandLayoutProviderConfig -> TaffyIO LayoutSnapshot
getHyprlandLayoutStateWith cfg = do
  (_, stateVar) <- getHyprlandLayoutStateChanAndVarWith cfg
  liftIO $ readMVar stateVar

buildHyprlandLayoutStateChanVar ::
  HyprlandLayoutProviderConfig ->
  TaffyIO HyprlandLayoutStateChanVar
buildHyprlandLayoutStateChanVar cfg = do
  stateChan <- liftIO newBroadcastTChanIO
  stateVar <- liftIO $ newMVar defaultHyprlandLayoutState
  taffyFork $ hyprlandLayoutStateLoop cfg stateChan stateVar
  pure $ HyprlandLayoutStateChanVar (stateChan, stateVar)

hyprlandLayoutStateLoop ::
  HyprlandLayoutProviderConfig ->
  TChan LayoutSnapshot ->
  MVar LayoutSnapshot ->
  TaffyIO ()
hyprlandLayoutStateLoop cfg stateChan stateVar = do
  refreshHyprlandLayoutState cfg stateChan stateVar
  hyprlandEventChan <- getHyprlandEventChan
  events <- liftIO $ Hypr.subscribeHyprlandEvents hyprlandEventChan
  forever $ do
    line <- liftIO $ atomically $ readTChan events
    when (layoutEventFilter cfg line) $
      refreshHyprlandLayoutState cfg stateChan stateVar

refreshHyprlandLayoutState ::
  HyprlandLayoutProviderConfig ->
  TChan LayoutSnapshot ->
  MVar LayoutSnapshot ->
  TaffyIO ()
refreshHyprlandLayoutState cfg stateChan stateVar = do
  previous <- liftIO $ readMVar stateVar
  name <-
    layoutSnapshotGetter cfg
      `catchAny` \err -> do
        wLog WARNING $ "Hyprland layout snapshot update failed: " <> show err
        pure $ layoutName previous
  let next =
        LayoutSnapshot
          { layoutBackend = LayoutBackendHyprland,
            layoutRevision = layoutRevision previous + 1,
            layoutName = name
          }
  liftIO $ do
    _ <- swapMVar stateVar next
    atomically $ writeTChan stateChan next

buildHyprlandLayoutSnapshot :: TaffyIO T.Text
buildHyprlandLayoutSnapshot = do
  client <- getHyprlandClient
  result <- liftIO $ HyprAPI.getHyprlandActiveWorkspace client
  case result of
    Left err ->
      wLog WARNING ("hyprctl activeworkspace failed: " <> show err) >> pure ""
    Right workspaceInfo ->
      pure $ fromMaybe "" $ HyprTypes.hyprActiveWorkspaceLayout workspaceInfo
