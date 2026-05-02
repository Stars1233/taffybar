{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : System.Taffybar.Widget.OmniMenu
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- A menu button for launcher and session-control commands. The button can wrap
-- any widget, so callers can use an icon from a theme, a file image, a label, or
-- their own composed image widget as the visible trigger.
module System.Taffybar.Widget.OmniMenu
  ( OmniMenuConfig (..),
    OmniMenuItem (..),
    OmniMenuSection (..),
    defaultOmniMenuConfig,
    omniMenuNew,
    omniMenuNewFromFile,
    omniMenuNewFromIconName,
    omniMenuNewWithConfig,
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Data.Foldable (foldlM)
import Data.Int (Int32)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Log.Logger
import System.Process
import System.Taffybar.Widget.Generic.AutoFillImage
import System.Taffybar.Widget.Generic.DynamicMenu
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.XDGMenu.Menu

data OmniMenuItem = OmniMenuItem
  { omniMenuItemLabel :: T.Text,
    omniMenuItemCommand :: T.Text,
    omniMenuItemIcon :: Maybe T.Text,
    omniMenuItemTooltip :: Maybe T.Text
  }

data OmniMenuSection = OmniMenuSection
  { omniMenuSectionLabel :: T.Text,
    omniMenuSectionItems :: [OmniMenuItem]
  }

data OmniMenuConfig = OmniMenuConfig
  { omniMenuClickWidget :: Gtk.Widget,
    omniMenuIncludeApplications :: Bool,
    omniMenuXDGMenuPrefix :: Maybe String,
    omniMenuSections :: [OmniMenuSection]
  }

defaultOmniMenuConfig :: Gtk.Widget -> OmniMenuConfig
defaultOmniMenuConfig clickWidget =
  OmniMenuConfig
    { omniMenuClickWidget = clickWidget,
      omniMenuIncludeApplications = True,
      omniMenuXDGMenuPrefix = Nothing,
      omniMenuSections = []
    }

omniMenuNew :: (MonadIO m) => Gtk.Widget -> [OmniMenuSection] -> m Gtk.Widget
omniMenuNew clickWidget sections =
  omniMenuNewWithConfig $
    (defaultOmniMenuConfig clickWidget)
      { omniMenuSections = sections
      }

omniMenuNewFromFile :: (MonadIO m) => FilePath -> [OmniMenuSection] -> m Gtk.Widget
omniMenuNewFromFile path sections = do
  image <- Gtk.imageNewFromFile path
  clickWidget <- Gtk.toWidget image
  omniMenuNew clickWidget sections

omniMenuNewFromIconName :: (MonadIO m) => T.Text -> [OmniMenuSection] -> m Gtk.Widget
omniMenuNewFromIconName iconName sections = do
  image <-
    Gtk.imageNewFromIconName
      (Just iconName)
      (fromIntegral $ fromEnum Gtk.IconSizeMenu)
  clickWidget <- Gtk.toWidget image
  omniMenuNew clickWidget sections

omniMenuNewWithConfig :: (MonadIO m) => OmniMenuConfig -> m Gtk.Widget
omniMenuNewWithConfig OmniMenuConfig {..} = do
  menuButton <-
    dynamicMenuNew
      DynamicMenuConfig
        { dmClickWidget = omniMenuClickWidget,
          dmPopulateMenu = populateOmniMenu
        }
  _ <- widgetSetClassGI menuButton "omni-menu-button"
  return menuButton
  where
    populateOmniMenu menu = do
      applicationsAdded <-
        if omniMenuIncludeApplications
          then addApplicationsMenu menu omniMenuXDGMenuPrefix
          else pure False
      void $ foldlM (addSection menu) applicationsAdded omniMenuSections
      Gtk.widgetShowAll menu

omniMenuLog :: Priority -> String -> IO ()
omniMenuLog = logM "System.Taffybar.Widget.OmniMenu"

omniMenuImageMenuItemNew ::
  T.Text -> (Int32 -> IO (Maybe Gdk.Pixbuf)) -> IO Gtk.MenuItem
omniMenuImageMenuItemNew labelText pixbufGetter = do
  box <- Gtk.boxNew Gtk.OrientationHorizontal 6
  iconSlot <- Gtk.boxNew Gtk.OrientationHorizontal 0
  label <- Gtk.labelNew $ Just labelText
  image <- autoFillImageNew pixbufGetter Gtk.OrientationHorizontal
  item <- Gtk.menuItemNew
  Gtk.widgetSetSizeRequest iconSlot omniMenuIconSize omniMenuIconSize
  Gtk.containerAdd iconSlot image
  Gtk.containerAdd box iconSlot
  Gtk.containerAdd box label
  Gtk.containerAdd item box
  Gtk.widgetSetHalign box Gtk.AlignStart
  Gtk.widgetSetHalign iconSlot Gtk.AlignCenter
  Gtk.widgetSetValign iconSlot Gtk.AlignCenter
  Gtk.widgetSetHalign image Gtk.AlignCenter
  Gtk.widgetSetValign image Gtk.AlignCenter
  Gtk.widgetSetValign label Gtk.AlignCenter
  Gtk.widgetSetValign box Gtk.AlignFill
  return item

omniMenuIconSize :: Int32
omniMenuIconSize = 16

addApplicationsMenu :: (Gtk.IsMenuShell menuShell) => menuShell -> Maybe String -> IO Bool
addApplicationsMenu menuShell menuPrefix = do
  xdgMenu <- buildMenu menuPrefix
  if isEmptyXDGMenu xdgMenu
    then pure False
    else do
      item <- Gtk.menuItemNewWithLabel "Applications"
      submenu <- Gtk.menuNew
      Gtk.menuItemSetSubmenu item (Just submenu)
      addXDGMenuContents submenu xdgMenu
      Gtk.menuShellAppend menuShell item
      pure True

isEmptyXDGMenu :: Menu -> Bool
isEmptyXDGMenu Menu {..} =
  null fmEntries && all isEmptyXDGMenu fmSubmenus

addXDGMenuContents :: (Gtk.IsMenuShell menuShell) => menuShell -> Menu -> IO ()
addXDGMenuContents menuShell Menu {..} = do
  mapM_ (addXDGSubmenu menuShell) fmSubmenus
  mapM_ (addXDGEntry menuShell) fmEntries

addXDGSubmenu :: (Gtk.IsMenuShell menuShell) => menuShell -> Menu -> IO ()
addXDGSubmenu menuShell menu@Menu {..} =
  unless (null fmEntries && null fmSubmenus) $ do
    item <-
      omniMenuImageMenuItemNew
        (T.pack fmName)
        (getImageForMaybeIconName (T.pack <$> fmIcon))
    submenu <- Gtk.menuNew
    Gtk.menuItemSetSubmenu item (Just submenu)
    addXDGMenuContents submenu menu
    Gtk.menuShellAppend menuShell item

addXDGEntry :: (Gtk.IsMenuShell menuShell) => menuShell -> MenuEntry -> IO ()
addXDGEntry menuShell MenuEntry {..} = do
  item <- omniMenuImageMenuItemNew feName (getImageForMaybeIconName feIcon)
  Gtk.widgetSetTooltipText item (Just feComment)
  Gtk.menuShellAppend menuShell item
  void $ Gtk.onMenuItemActivate item $ do
    omniMenuLog DEBUG $ "Launching '" ++ feCommand ++ "'"
    void $ spawnCommand feCommand

addSection :: (Gtk.IsMenuShell menuShell) => menuShell -> Bool -> OmniMenuSection -> IO Bool
addSection menuShell hasPrevious OmniMenuSection {..} =
  if null omniMenuSectionItems
    then pure hasPrevious
    else do
      when hasPrevious $ addSeparator menuShell
      addSectionHeader menuShell omniMenuSectionLabel
      mapM_ (addCommandItem menuShell) omniMenuSectionItems
      pure True

addSeparator :: (Gtk.IsMenuShell menuShell) => menuShell -> IO ()
addSeparator menuShell = do
  separator <- Gtk.separatorMenuItemNew
  Gtk.menuShellAppend menuShell separator

addSectionHeader :: (Gtk.IsMenuShell menuShell) => menuShell -> T.Text -> IO ()
addSectionHeader menuShell label = do
  item <- Gtk.menuItemNewWithLabel label
  Gtk.widgetSetSensitive item False
  Gtk.menuShellAppend menuShell item

addCommandItem :: (Gtk.IsMenuShell menuShell) => menuShell -> OmniMenuItem -> IO ()
addCommandItem menuShell OmniMenuItem {..} = do
  item <-
    omniMenuImageMenuItemNew
      omniMenuItemLabel
      (getImageForMaybeIconName omniMenuItemIcon)
  Gtk.widgetSetTooltipText item omniMenuItemTooltip
  Gtk.menuShellAppend menuShell item
  void $ Gtk.onMenuItemActivate item $ do
    omniMenuLog DEBUG $ "Launching '" ++ T.unpack omniMenuItemCommand ++ "'"
    void $ spawnCommand $ T.unpack omniMenuItemCommand
