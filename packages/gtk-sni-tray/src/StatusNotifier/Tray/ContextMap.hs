module StatusNotifier.Tray.ContextMap
  ( ContextEntry (..),
    deleteContext,
    lookupReadyContext,
    readyContexts,
    reserveContext,
    setReadyContext,
  )
where

import Control.Monad ((>=>))
import qualified Data.Map.Strict as Map

data ContextEntry a
  = ContextPending
  | ContextReady a
  deriving (Eq, Show)

reserveContext :: (Ord k) => k -> Map.Map k (ContextEntry a) -> (Bool, Map.Map k (ContextEntry a))
reserveContext key contexts
  | Map.member key contexts = (False, contexts)
  | otherwise = (True, Map.insert key ContextPending contexts)

setReadyContext :: (Ord k) => k -> a -> Map.Map k (ContextEntry a) -> Map.Map k (ContextEntry a)
setReadyContext key value = Map.insert key (ContextReady value)

lookupReadyContext :: (Ord k) => k -> Map.Map k (ContextEntry a) -> Maybe a
lookupReadyContext key = Map.lookup key >=> readyContext
  where
    readyContext ContextPending = Nothing
    readyContext (ContextReady value) = Just value

readyContexts :: Map.Map k (ContextEntry a) -> Map.Map k a
readyContexts = Map.mapMaybe readyContext
  where
    readyContext ContextPending = Nothing
    readyContext (ContextReady value) = Just value

deleteContext :: (Ord k) => k -> Map.Map k (ContextEntry a) -> Map.Map k (ContextEntry a)
deleteContext = Map.delete
