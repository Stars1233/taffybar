{-# LANGUAGE TemplateHaskell #-}

module DBusMenu.Client where

import DBus.Generation
import DBusMenu.Client.Util
import System.FilePath

-- Generates DBus client functions/signals for the com.canonical.dbusmenu
-- interface from introspection XML, similar to Taffybar's approach.
generateClientFromFile
  defaultRecordGenerationParams
  defaultGenerationParams {genTakeSignalErrorHandler = True}
  False
  ("dbus-xml" </> "com.canonical.dbusmenu.xml")
