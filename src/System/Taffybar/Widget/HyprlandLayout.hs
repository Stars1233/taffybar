{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.HyprlandLayout
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that shows the Hyprland layout used in the currently
-- active workspace.
module System.Taffybar.Widget.HyprlandLayout
  ( HyprlandLayoutConfig (..),
    defaultHyprlandLayoutConfig,
    hyprlandLayoutNew,
  )
where

import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import qualified Data.Text as T
import GI.Gdk
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority (..))
import System.Taffybar.Context
import System.Taffybar.Hyprland
  ( runHyprlandCommandRawT,
  )
import qualified System.Taffybar.Information.Hyprland as Hypr
import System.Taffybar.Information.Layout.Hyprland
  ( getHyprlandLayoutStateChanAndVar,
  )
import System.Taffybar.Information.Layout.Model
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Util

-- | Configuration for 'hyprlandLayoutNew'.
data HyprlandLayoutConfig = HyprlandLayoutConfig
  { formatLayout :: T.Text -> TaffyIO T.Text,
    -- | Retained for API compatibility; the widget is now event-driven.
    updateIntervalSeconds :: Double,
    onLeftClick :: Maybe [String],
    onRightClick :: Maybe [String]
  }

instance Default HyprlandLayoutConfig where
  def = defaultHyprlandLayoutConfig

-- | Default Hyprland layout widget configuration.
defaultHyprlandLayoutConfig :: HyprlandLayoutConfig
defaultHyprlandLayoutConfig =
  HyprlandLayoutConfig
    { formatLayout = return,
      updateIntervalSeconds = 1,
      onLeftClick = Nothing,
      onRightClick = Nothing
    }

-- | Create a new Hyprland Layout widget.
hyprlandLayoutNew :: HyprlandLayoutConfig -> TaffyIO Gtk.Widget
hyprlandLayoutNew config = do
  ctx <- ask
  (stateChan, stateVar) <- hyprlandLayoutStateSource
  initialSnapshot <- liftIO $ MV.readMVar stateVar
  label <- lift $ Gtk.labelNew (Nothing :: Maybe T.Text)
  _ <- widgetSetClassGI label "layout-label"

  let renderSnapshot snapshot = do
        markup <- formatLayout config (layoutName snapshot)
        lift $ Gtk.labelSetMarkup label markup

  void $ renderSnapshot initialSnapshot

  ebox <- lift Gtk.eventBoxNew
  lift $ Gtk.containerAdd ebox label
  _ <- liftIO $ Gtk.onWidgetRealize ebox $ do
    latestSnapshot <- MV.readMVar stateVar
    void $ runReaderT (renderSnapshot latestSnapshot) ctx
  _ <-
    liftIO $
      channelWidgetNew
        ebox
        stateChan
        (\snapshot -> postGUIASync $ runReaderT (renderSnapshot snapshot) ctx)
  _ <- lift $ Gtk.onWidgetButtonPressEvent ebox $ dispatchButtonEvent ctx config
  lift $ Gtk.widgetShowAll ebox
  Gtk.toWidget ebox

hyprlandLayoutStateSource :: TaffyIO (TChan LayoutSnapshot, MV.MVar LayoutSnapshot)
hyprlandLayoutStateSource = getHyprlandLayoutStateChanAndVar

-- | Call the configured dispatch action depending on click.
dispatchButtonEvent :: Context -> HyprlandLayoutConfig -> EventButton -> IO Bool
dispatchButtonEvent context config btn = do
  pressType <- getEventButtonType btn
  buttonNumber <- getEventButtonButton btn
  case pressType of
    EventTypeButtonPress ->
      case buttonNumber of
        1 -> runReaderT (dispatchMaybe $ onLeftClick config) context >> return True
        3 -> runReaderT (dispatchMaybe $ onRightClick config) context >> return True
        _ -> return False
    _ -> return False

-- | Dispatch a Hyprland command if provided.
dispatchMaybe :: Maybe [String] -> TaffyIO ()
dispatchMaybe maybeArgs =
  case maybeArgs of
    Nothing -> return ()
    Just args -> do
      result <- runHyprlandCommandRawT (Hypr.hyprCommand ("dispatch" : args))
      case result of
        Left err ->
          logPrintF
            "System.Taffybar.Widget.HyprlandLayout"
            WARNING
            "Failed to dispatch Hyprland command: %s"
            (show err)
        Right _ -> return ()
