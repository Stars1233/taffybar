module System.Taffybar.Information.LayoutSpec (spec) where

import Data.Text qualified as T
import System.Taffybar.Information.Layout.Hyprland
  ( isRelevantHyprlandLayoutEvent,
  )
import Test.Hspec

spec :: Spec
spec = describe "isRelevantHyprlandLayoutEvent" $ do
  it "accepts workspace and reconnect events" $ do
    isRelevantHyprlandLayoutEvent "workspace>>2" `shouldBe` True
    isRelevantHyprlandLayoutEvent "focusedmon>>HDMI-A-1,2" `shouldBe` True
    isRelevantHyprlandLayoutEvent "taffybar-hyprland-connected>>" `shouldBe` True

  it "ignores unrelated events" $ do
    isRelevantHyprlandLayoutEvent "activewindow>>kitty" `shouldBe` False
    isRelevantHyprlandLayoutEvent (T.pack "openwindow>>0x123") `shouldBe` False
