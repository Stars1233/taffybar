module ContextMapSpec (spec) where

import qualified Data.Map.Strict as Map
import StatusNotifier.Tray.ContextMap
import Test.Hspec

spec :: Spec
spec = describe "StatusNotifier.Tray.ContextMap" $ do
  it "reserves a new key once and blocks duplicates until ready" $ do
    let (reserved1, contexts1) = reserveContext "item-a" Map.empty
        (reserved2, contexts2) = reserveContext "item-a" contexts1
    reserved1 `shouldBe` True
    reserved2 `shouldBe` False
    lookupReadyContext "item-a" contexts2 `shouldBe` (Nothing :: Maybe String)

  it "exposes a value once the reservation is marked ready" $ do
    let (_, contexts1) = reserveContext "item-a" Map.empty
        contexts2 = setReadyContext "item-a" ("ready" :: String) contexts1
    lookupReadyContext "item-a" contexts2 `shouldBe` Just "ready"
    readyContexts contexts2 `shouldBe` Map.fromList [("item-a", "ready")]

  it "allows a fresh reservation after deleting a pending entry" $ do
    let (_, contexts1) = reserveContext "item-a" Map.empty
        contexts2 = deleteContext "item-a" contexts1
        (reservedAgain, _) = reserveContext "item-a" contexts2
    reservedAgain `shouldBe` True
