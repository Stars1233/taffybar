module Main (main) where

import qualified HostSpec
import Test.Hspec
import qualified UtilSpec
import qualified WatcherSpec

main :: IO ()
main = hspec $ do
  UtilSpec.spec
  WatcherSpec.spec
  HostSpec.spec
