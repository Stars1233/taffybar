module ContextMapSpec (spec) where

import qualified Data.Map.Strict as Map
import StatusNotifier.Tray.ContextMap
import Test.Hspec

spec :: Spec
spec = describe "StatusNotifier.Tray.ContextMap" $ do
  it "reserves a new key once and blocks duplicates until ready" $ do
    let (reserved1, contexts1) = reserveContext "item-a" empty
        (reserved2, contexts2) = reserveContext "item-a" contexts1
    reserved1 `shouldSatisfy` (/= Nothing)
    reserved2 `shouldBe` Nothing
    lookupReadyContext "item-a" contexts2 `shouldBe` (Nothing :: Maybe String)

  it "exposes a value once the reservation is marked ready" $ do
    let (Just reservation, contexts1) = reserveContext "item-a" empty
        (didFinalize, contexts2) =
          setReadyContext "item-a" reservation ("ready" :: String) contexts1
    didFinalize `shouldBe` True
    lookupReadyContext "item-a" contexts2 `shouldBe` Just "ready"
    readyContexts contexts2 `shouldBe` Map.fromList [("item-a", "ready")]

  it "allows a fresh reservation after deleting a pending entry" $ do
    let (Just reservation1, contexts1) = reserveContext "item-a" empty
        contexts2 = deleteContext "item-a" contexts1
        contexts3 = cancelReservation "item-a" reservation1 contexts2
        (reservedAgain, _) = reserveContext "item-a" contexts3
    reservedAgain `shouldSatisfy` (/= Nothing)

  it "rejects a stale reservation after remove and re-add" $ do
    let (Just reservation1, contexts1) = reserveContext "item-a" empty
        contexts2 = deleteContext "item-a" contexts1
        (Just reservation2, contexts3) = reserveContext "item-a" contexts2
        (didFinalizeOld, contexts4) =
          setReadyContext "item-a" reservation1 ("stale" :: String) contexts3
        (didFinalizeNew, contexts5) =
          setReadyContext "item-a" reservation2 "fresh" contexts4
    didFinalizeOld `shouldBe` False
    didFinalizeNew `shouldBe` True
    lookupReadyContext "item-a" contexts5 `shouldBe` Just "fresh"
