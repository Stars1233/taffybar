module Main where

import DBus.Client
import StatusNotifier.Watcher.Client

main :: IO ()
main = do
  client <- connectSession
  registeredItems <-
    getRegisteredSNIEntries
      client
  print registeredItems
