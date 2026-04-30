{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Taffybar.ContextSpec
  ( spec,

    -- * Utils
    runTaffyDefault,

    -- * Abstract Config
    GenSimpleConfig (..),
    toSimpleConfig,
    GenWidget (..),
    toTaffyWidget,
    GenSpace (..),
    GenCssPath (..),
    toCssPaths,
    GenMonitorsAction (..),
    toMonitorsAction,
  )
where

import Control.Exception (SomeException, bracket, catch)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Default (def)
import Data.Ratio ((%))
import GHC.Generics (Generic)
import GI.Gtk (Widget)
import Network.Socket qualified as Socket
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Taffybar.Context
import System.Taffybar.Context.Backend (prepareBackendEnvironment)
import System.Taffybar.SimpleConfig
import System.Taffybar.Test.DBusSpec (withTestDBus)
import System.Taffybar.Test.UtilSpec (logSetup, withEnv, withSetEnv)
import System.Taffybar.Test.XvfbSpec (setDefaultDisplay_, withXdummy)
import System.Taffybar.Widget.SimpleClock (textClockNewWith)
import System.Taffybar.Widget.Workspaces (workspacesNew)
import Test.Hspec hiding (context)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = logSetup $ sequential $ aroundAll_ withTestDBus $ aroundAll_ (withXdummy . flip setDefaultDisplay_) $ do
  describe "detectBackend" $ do
    it "prefers an explicit X11 session over a discovered Wayland socket" $ do
      tmp <- getTemporaryDirectory
      let runtime = tmp </> "taffybar-test-runtime-x11-with-wayland-socket"
          wlPath = runtime </> "wayland-0"
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())
      createDirectoryIfMissing True runtime
      withUnixSocket wlPath
        $ withEnv
          [ ("XDG_RUNTIME_DIR", const $ Just runtime),
            ("DISPLAY", const $ Just ":0"),
            ("XDG_SESSION_TYPE", const $ Just "x11"),
            ("WAYLAND_DISPLAY", const Nothing),
            ("HYPRLAND_INSTANCE_SIGNATURE", const Nothing)
          ]
        $ detectBackend `shouldReturn` BackendX11
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())

    it "falls back to X11 when WAYLAND_DISPLAY is set but the socket is missing" $ do
      tmp <- getTemporaryDirectory
      let runtime = tmp </> "taffybar-test-runtime-missing-socket"
          wl = "wayland-stale"
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())
      createDirectoryIfMissing True runtime
      withSetEnv
        [ ("XDG_RUNTIME_DIR", runtime),
          ("WAYLAND_DISPLAY", wl),
          ("XDG_SESSION_TYPE", "wayland")
        ]
        $ do
          detectBackend `shouldReturn` BackendX11
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())

    it "falls back to X11 when WAYLAND_DISPLAY points at a non-socket path" $ do
      tmp <- getTemporaryDirectory
      let runtime = tmp </> "taffybar-test-runtime-non-socket"
          wl = "wayland-stale"
          wlPath = runtime </> wl
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())
      createDirectoryIfMissing True runtime
      writeFile wlPath "" -- exists but is not a socket
      withSetEnv
        [ ("XDG_RUNTIME_DIR", runtime),
          ("WAYLAND_DISPLAY", wl),
          ("XDG_SESSION_TYPE", "wayland")
        ]
        $ do
          detectBackend `shouldReturn` BackendX11
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())

  describe "prepareBackendEnvironment" $ do
    it "replaces a stale WAYLAND_DISPLAY with a live discovered socket" $ do
      tmp <- getTemporaryDirectory
      let runtime = tmp </> "taffybar-test-runtime-repair-wayland"
          liveWayland = "wayland-live"
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())
      createDirectoryIfMissing True runtime
      withUnixSocket (runtime </> liveWayland)
        $ withSetEnv
          [ ("XDG_RUNTIME_DIR", runtime),
            ("WAYLAND_DISPLAY", "wayland-stale"),
            ("XDG_SESSION_TYPE", "wayland")
          ]
        $ do
          prepareBackendEnvironment
          lookupEnv "WAYLAND_DISPLAY" `shouldReturn` Just liveWayland
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())

    it "replaces a stale HYPRLAND_INSTANCE_SIGNATURE with a live discovered signature" $ do
      tmp <- getTemporaryDirectory
      let runtime = tmp </> "taffybar-test-runtime-repair-hyprland"
          liveSig = "hyprland-live"
          liveDir = runtime </> "hypr" </> liveSig
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())
      createDirectoryIfMissing True liveDir
      withUnixSocket (liveDir </> ".socket.sock")
        $ withSetEnv
          [ ("XDG_RUNTIME_DIR", runtime),
            ("XDG_SESSION_TYPE", "wayland"),
            ("HYPRLAND_INSTANCE_SIGNATURE", "hyprland-stale")
          ]
        $ do
          prepareBackendEnvironment
          lookupEnv "HYPRLAND_INSTANCE_SIGNATURE" `shouldReturn` Just liveSig
      removePathForcibly runtime `catch` (\(_ :: SomeException) -> pure ())

  describe "Fuzz tests" $ do
    prop "eval generators" prop_genSimpleConfig
    xprop "TaffybarConfig" prop_taffybarConfig

------------------------------------------------------------------------

withUnixSocket :: FilePath -> IO a -> IO a
withUnixSocket path action =
  bracket
    (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
    Socket.close
    $ \sock -> do
      Socket.bind sock (Socket.SockAddrUnix path)
      Socket.listen sock 1
      action

------------------------------------------------------------------------

runTaffyDefault :: TaffyIO a -> IO a
runTaffyDefault f = buildContext def >>= runReaderT f

------------------------------------------------------------------------

-- | Represents 'SimpleTaffyConfig' in a more abstract way, so that
-- it's easier to 'show', 'shrink', 'assert', etc.
data GenSimpleConfig = GenSimpleConfig
  { monitors :: GenMonitorsAction,
    size :: StrutSize,
    padding :: GenSpace,
    position :: Position,
    spacing :: GenSpace,
    start :: [GenWidget],
    center :: [GenWidget],
    end :: [GenWidget],
    css :: [GenCssPath]
  }
  deriving (Show, Eq, Generic)

-- | Build an actual taffy config from the abstract form.
toSimpleConfig :: GenSimpleConfig -> SimpleTaffyConfig
toSimpleConfig GenSimpleConfig {..} =
  SimpleTaffyConfig
    { monitorsAction = toMonitorsAction monitors,
      barHeight = size,
      barPadding = unGenSpace padding,
      barPosition = position,
      widgetSpacing = unGenSpace spacing,
      startWidgets = map toTaffyWidget start,
      centerWidgets = map toTaffyWidget center,
      endWidgets = map toTaffyWidget end,
      barLevels = Nothing,
      cssPaths = toCssPaths css,
      startupHook = pure () -- TODO: add something
    }

toTaffyWidget :: GenWidget -> TaffyIO Widget
toTaffyWidget = \case
  WorkspacesWidget -> workspacesNew def
  ClockWidget -> textClockNewWith def

toCssPaths :: [GenCssPath] -> [FilePath]
toCssPaths = map (\p -> "fixme_" ++ show p ++ ".css")

toMonitorsAction :: GenMonitorsAction -> TaffyIO [Int]
toMonitorsAction = \case
  UsePrimaryMonitor -> usePrimaryMonitor
  UseAllMonitors -> useAllMonitors
  UseTheseMonitors xs -> pure xs

instance Arbitrary GenSimpleConfig where
  arbitrary = GenSimpleConfig <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary StrutSize where
  arbitrary =
    oneof
      [ ExactSize . getSmall . getPositive <$> arbitrary,
        ScreenRatio <$> elements [1 % 27, 1 % 50, 1 % 2] -- TODO: more arbitrary
      ]
  shrink (ExactSize s) = ExactSize . getPositive <$> shrink (Positive (fromIntegral s))
  shrink (ScreenRatio r) = ScreenRatio <$> shrink r

instance Arbitrary Position where
  arbitrary = arbitraryBoundedEnum
  shrink Top = []
  shrink Bottom = [Top]

newtype GenSpace = GenSpace {unGenSpace :: Int}
  deriving (Show, Read, Eq, Generic)

instance Arbitrary GenSpace where
  arbitrary = GenSpace . getSmall . getPositive <$> arbitrary
  shrink = genericShrink

data GenWidget = WorkspacesWidget | ClockWidget
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Arbitrary GenWidget where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

data GenCssPath = RedStyle | BlueStyle | MissingCss | FaultyCss
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Arbitrary GenCssPath where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

data GenMonitorsAction
  = UsePrimaryMonitor
  | UseAllMonitors
  | UseTheseMonitors [Int]
  deriving (Show, Read, Eq, Generic)

instance Arbitrary GenMonitorsAction where
  arbitrary =
    oneof
      [ pure UsePrimaryMonitor,
        pure UseAllMonitors,
        wild
      ]
    where
      -- This could be a lot meaner.
      wild = do
        NonNegative (Small n) <- arbitrary
        pure (UseTheseMonitors [0 .. n])
  shrink = genericShrink

------------------------------------------------------------------------

prop_genSimpleConfig :: GenSimpleConfig -> Property
prop_genSimpleConfig cfg =
  checkCoverage $
    cover 25 (monitors cfg == UsePrimaryMonitor) "Primary monitor only" $
      cfg === cfg

prop_taffybarConfig :: GenSimpleConfig -> Property
prop_taffybarConfig cfg =
  within 1_000_000 $
    monadicIO $
      pure (cfg =/= cfg)

-- Some possible assertions:
--   startupHook executed exactly once
--   css rules are applied
--   css files later in list have precedence
--   missing css => exception
--   error in css => warning and continue
--   widgets are visible
--   spacing/height/position/padding are observed
--   appears on the correct monitor
