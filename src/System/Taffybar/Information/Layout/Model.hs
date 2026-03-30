-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Layout.Model
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Backend-agnostic layout model shared by layout widgets and providers.
module System.Taffybar.Information.Layout.Model
  ( LayoutBackend (..),
    LayoutSnapshot (..),
  )
where

import Data.Text (Text)
import Data.Word (Word64)

data LayoutBackend
  = LayoutBackendXMonad
  | LayoutBackendHyprland
  deriving (Eq, Show)

data LayoutSnapshot = LayoutSnapshot
  { layoutBackend :: LayoutBackend,
    layoutRevision :: Word64,
    layoutName :: Text
  }
  deriving (Eq, Show)
