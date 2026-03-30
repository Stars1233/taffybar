{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Layout.EWMH
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared XMonad layout provider using an X11 property subscription and a
-- broadcast channel + state MVar.
module System.Taffybar.Information.Layout.EWMH
  ( EWMHLayoutProviderConfig (..),
    defaultEWMHLayoutProviderConfig,
    defaultEWMHLayoutState,
    getEWMHLayoutStateChanAndVar,
    getEWMHLayoutStateChanAndVarWith,
    getEWMHLayoutState,
    getEWMHLayoutStateWith,
    switchEWMHLayoutBy,
    xLayoutProp,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception.Enclosed (catchAny)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.Text as T
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context
  ( TaffyIO,
    getStateDefault,
    runX11Def,
    subscribeToPropertyEvents,
    taffyFork,
  )
import System.Taffybar.Information.Layout.Model
import System.Taffybar.Information.X11DesktopInfo
  ( X11Property,
    getAtom,
    readAsString,
    sendCommandEvent,
  )

data EWMHLayoutProviderConfig = EWMHLayoutProviderConfig
  { layoutSnapshotGetter :: TaffyIO T.Text,
    layoutUpdateEvents :: [String]
  }

defaultEWMHLayoutProviderConfig :: EWMHLayoutProviderConfig
defaultEWMHLayoutProviderConfig =
  EWMHLayoutProviderConfig
    { layoutSnapshotGetter = buildEWMHLayoutSnapshot,
      layoutUpdateEvents = [xLayoutProp]
    }

defaultEWMHLayoutState :: LayoutSnapshot
defaultEWMHLayoutState =
  LayoutSnapshot
    { layoutBackend = LayoutBackendXMonad,
      layoutRevision = 0,
      layoutName = ""
    }

newtype EWMHLayoutStateChanVar
  = EWMHLayoutStateChanVar
      (TChan LayoutSnapshot, MVar LayoutSnapshot)

wLog :: (MonadIO m) => Priority -> String -> m ()
wLog level message =
  liftIO $ logM "System.Taffybar.Information.Layout.EWMH" level message

getEWMHLayoutStateChanAndVar ::
  TaffyIO (TChan LayoutSnapshot, MVar LayoutSnapshot)
getEWMHLayoutStateChanAndVar =
  getEWMHLayoutStateChanAndVarWith defaultEWMHLayoutProviderConfig

getEWMHLayoutStateChanAndVarWith ::
  EWMHLayoutProviderConfig ->
  TaffyIO (TChan LayoutSnapshot, MVar LayoutSnapshot)
getEWMHLayoutStateChanAndVarWith cfg = do
  EWMHLayoutStateChanVar chanAndVar <- getStateDefault $ buildEWMHLayoutStateChanVar cfg
  pure chanAndVar

getEWMHLayoutState :: TaffyIO LayoutSnapshot
getEWMHLayoutState =
  getEWMHLayoutStateWith defaultEWMHLayoutProviderConfig

getEWMHLayoutStateWith :: EWMHLayoutProviderConfig -> TaffyIO LayoutSnapshot
getEWMHLayoutStateWith cfg = do
  (_, stateVar) <- getEWMHLayoutStateChanAndVarWith cfg
  liftIO $ readMVar stateVar

buildEWMHLayoutStateChanVar ::
  EWMHLayoutProviderConfig ->
  TaffyIO EWMHLayoutStateChanVar
buildEWMHLayoutStateChanVar cfg = do
  stateChan <- liftIO newBroadcastTChanIO
  stateVar <- liftIO $ newMVar defaultEWMHLayoutState
  taffyFork $ ewmhLayoutStateLoop cfg stateChan stateVar
  pure $ EWMHLayoutStateChanVar (stateChan, stateVar)

ewmhLayoutStateLoop ::
  EWMHLayoutProviderConfig ->
  TChan LayoutSnapshot ->
  MVar LayoutSnapshot ->
  TaffyIO ()
ewmhLayoutStateLoop cfg stateChan stateVar = do
  refreshEWMHLayoutState cfg stateChan stateVar
  setupEWMHLayoutSubscription cfg stateChan stateVar
    `catchAny` \err ->
      wLog WARNING $ "Failed to subscribe to XMonad layout events: " <> show err

setupEWMHLayoutSubscription ::
  EWMHLayoutProviderConfig ->
  TChan LayoutSnapshot ->
  MVar LayoutSnapshot ->
  TaffyIO ()
setupEWMHLayoutSubscription cfg stateChan stateVar = do
  ctx <- ask
  let refreshForEvent _ =
        runReaderT (refreshEWMHLayoutState cfg stateChan stateVar) ctx
  _ <- subscribeToPropertyEvents (layoutUpdateEvents cfg) (liftIO . void . refreshForEvent)
  pure ()

refreshEWMHLayoutState ::
  EWMHLayoutProviderConfig ->
  TChan LayoutSnapshot ->
  MVar LayoutSnapshot ->
  TaffyIO ()
refreshEWMHLayoutState cfg stateChan stateVar = do
  previous <- liftIO $ readMVar stateVar
  name <-
    layoutSnapshotGetter cfg
      `catchAny` \err -> do
        wLog WARNING $ "XMonad layout snapshot update failed: " <> show err
        pure $ layoutName previous
  let next =
        LayoutSnapshot
          { layoutBackend = LayoutBackendXMonad,
            layoutRevision = layoutRevision previous + 1,
            layoutName = name
          }
  liftIO $ do
    _ <- swapMVar stateVar next
    atomically $ writeTChan stateChan next

buildEWMHLayoutSnapshot :: TaffyIO T.Text
buildEWMHLayoutSnapshot =
  T.pack <$> runX11Def "" (readAsString Nothing xLayoutProp)

xLayoutProp :: String
xLayoutProp = "_XMONAD_CURRENT_LAYOUT"

switchEWMHLayoutBy :: Int -> X11Property ()
switchEWMHLayoutBy amount = do
  cmd <- getAtom xLayoutProp
  sendCommandEvent cmd (fromIntegral amount)
