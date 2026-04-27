module Main where

import DBus.Client
import Data.String
import StatusNotifier.Watcher.Client

main = do
  client <- connectSession
  registeredItems <-
    getRegisteredSNIEntries
      client
  print registeredItems
  return ()
