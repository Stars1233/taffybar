{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Layout
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that shows the current layout using a backend-specific
-- information provider. Under X11/XMonad it retains the historical click
-- behavior for layout switching.
module System.Taffybar.Widget.Layout
  ( -- * Usage
    -- $usage
    LayoutConfig (..),
    defaultLayoutConfig,
    layoutNew,
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
import System.Taffybar.Context
import System.Taffybar.Information.Layout.EWMH
  ( getEWMHLayoutStateChanAndVar,
    switchEWMHLayoutBy,
  )
import System.Taffybar.Information.Layout.Hyprland
  ( getHyprlandLayoutStateChanAndVar,
  )
import System.Taffybar.Information.Layout.Model
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Util

-- $usage
--
-- This widget requires that the "XMonad.Hooks.TaffybarPagerHints" hook be
-- installed in your @xmonad.hs@:
--
-- > import XMonad.Hooks.TaffybarPagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.Widget.Layout
-- > main = do
-- >   let los = layoutSwitcherNew def
--
-- now you can use @los@ as any other Taffybar widget.

-- | Configuration for how the current layout name is rendered.
newtype LayoutConfig = LayoutConfig
  { formatLayout :: T.Text -> TaffyIO T.Text
  }

-- | Default layout formatting: display the layout name unchanged.
defaultLayoutConfig :: LayoutConfig
defaultLayoutConfig = LayoutConfig return

instance Default LayoutConfig where
  def = defaultLayoutConfig

layoutNew :: LayoutConfig -> TaffyIO Gtk.Widget
layoutNew config = do
  ctx <- ask
  backendType <- asks backend
  (stateChan, stateVar) <- autoLayoutStateSource
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
  _ <- lift $ Gtk.onWidgetButtonPressEvent ebox $ dispatchButtonEvent ctx backendType
  lift $ Gtk.widgetShowAll ebox
  Gtk.toWidget ebox

autoLayoutStateSource :: TaffyIO (TChan LayoutSnapshot, MV.MVar LayoutSnapshot)
autoLayoutStateSource = do
  backendType <- asks backend
  case backendType of
    BackendWayland -> getHyprlandLayoutStateChanAndVar
    BackendX11 -> getEWMHLayoutStateChanAndVar

dispatchButtonEvent :: Context -> Backend -> EventButton -> IO Bool
dispatchButtonEvent context backendType btn = do
  pressType <- getEventButtonType btn
  buttonNumber <- getEventButtonButton btn
  case pressType of
    EventTypeButtonPress -> case backendType of
      BackendWayland -> return False
      BackendX11 ->
        case buttonNumber of
          1 -> runReaderT (runX11Def () (switchEWMHLayoutBy 1)) context >> return True
          2 -> runReaderT (runX11Def () (switchEWMHLayoutBy (-1))) context >> return True
          _ -> return False
    _ -> return False
