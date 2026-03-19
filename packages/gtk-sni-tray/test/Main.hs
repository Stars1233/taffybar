module Main (main) where

import qualified ContextMapSpec
import Test.Hspec

main :: IO ()
main = hspec ContextMapSpec.spec
