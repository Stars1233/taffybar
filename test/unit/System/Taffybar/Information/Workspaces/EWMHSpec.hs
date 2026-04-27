module System.Taffybar.Information.Workspaces.EWMHSpec (spec) where

import System.Taffybar.Information.Workspaces.EWMH
  ( defaultEWMHWorkspaceProviderConfig,
    workspaceUpdateEvents,
  )
import System.Taffybar.Information.X11DesktopInfo (xmonadVisibleWorkspaces)
import Test.Hspec

spec :: Spec
spec =
  describe "defaultEWMHWorkspaceProviderConfig" $
    it "subscribes to xmonad visible workspace updates" $
      xmonadVisibleWorkspaces `elem` workspaceUpdateEvents defaultEWMHWorkspaceProviderConfig
        `shouldBe` True
