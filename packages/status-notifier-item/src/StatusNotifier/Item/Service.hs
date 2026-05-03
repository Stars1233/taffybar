{-# LANGUAGE OverloadedStrings #-}

module StatusNotifier.Item.Service where

import DBus
import DBus.Client
import qualified Data.ByteString as BS
import Data.Int
import Data.String
import qualified StatusNotifier.Watcher.Client as W

data ItemParams = ItemParams
  { iconName :: String,
    iconOverlayName :: String,
    itemDBusName :: String
  }
  deriving (Eq, Show, Read)

buildItem :: ItemParams -> IO (Either MethodError ())
buildItem
  ItemParams
    { iconName = name,
      iconOverlayName = overlayName,
      itemDBusName = itemBusName
    } = do
    client <- connectSession
    let getTooltip :: IO (String, [(Int32, Int32, BS.ByteString)], String, String)
        getTooltip = return ("", [], "Title", "Text")
    let clientInterface =
          Interface
            { interfaceName = "org.kde.StatusNotifierItem",
              interfaceMethods = [],
              interfaceProperties =
                [ readOnlyProperty "IconName" $ return name,
                  readOnlyProperty "OverlayIconName" $ return overlayName,
                  readOnlyProperty "ToolTip" getTooltip
                ],
              interfaceSignals = []
            }
    export client (fromString "/StatusNotifierItem") clientInterface
    _ <- requestName client (busName_ itemBusName) []
    W.registerStatusNotifierItem client itemBusName
