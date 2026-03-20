module StatusNotifier.Tray.ContextMap
  ( ContextEntry (..),
    ContextMap,
    Reservation,
    cancelReservation,
    deleteContext,
    empty,
    lookupReadyContext,
    readyContexts,
    reserveContext,
    setReadyContext,
  )
where

import Control.Monad ((>=>))
import qualified Data.Map.Strict as Map

newtype Reservation = Reservation Int
  deriving (Eq, Ord, Show)

data ContextEntry a
  = ContextPending Reservation
  | ContextReady a
  deriving (Eq, Show)

data ContextMap k a = ContextMap
  { nextReservation :: !Int,
    contextEntries :: Map.Map k (ContextEntry a)
  }
  deriving (Eq, Show)

empty :: ContextMap k a
empty = ContextMap {nextReservation = 0, contextEntries = Map.empty}

reserveContext ::
  (Ord k) =>
  k ->
  ContextMap k a ->
  (Maybe Reservation, ContextMap k a)
reserveContext key contexts@(ContextMap {nextReservation = nextReservation', contextEntries = contextEntries'})
  | Map.member key contextEntries' = (Nothing, contexts)
  | otherwise =
      let reservation = Reservation nextReservation'
       in ( Just reservation,
            ContextMap
              { nextReservation = nextReservation' + 1,
                contextEntries = Map.insert key (ContextPending reservation) contextEntries'
              }
          )

setReadyContext ::
  (Ord k) =>
  k ->
  Reservation ->
  a ->
  ContextMap k a ->
  (Bool, ContextMap k a)
setReadyContext key reservation value contexts@(ContextMap {contextEntries = contextEntries'})
  | isActiveReservation key reservation contexts =
      (True, contexts {contextEntries = Map.insert key (ContextReady value) contextEntries'})
  | otherwise = (False, contexts)

lookupReadyContext :: (Ord k) => k -> ContextMap k a -> Maybe a
lookupReadyContext key = Map.lookup key . contextEntries >=> readyContext
  where
    readyContext (ContextPending _) = Nothing
    readyContext (ContextReady value) = Just value

readyContexts :: ContextMap k a -> Map.Map k a
readyContexts = Map.mapMaybe readyContext . contextEntries
  where
    readyContext (ContextPending _) = Nothing
    readyContext (ContextReady value) = Just value

deleteContext :: (Ord k) => k -> ContextMap k a -> ContextMap k a
deleteContext key contexts@(ContextMap {contextEntries = contextEntries'}) =
  contexts {contextEntries = Map.delete key contextEntries'}

cancelReservation ::
  (Ord k) =>
  k ->
  Reservation ->
  ContextMap k a ->
  ContextMap k a
cancelReservation key reservation contexts
  | isActiveReservation key reservation contexts = deleteContext key contexts
  | otherwise = contexts

isActiveReservation :: (Ord k) => k -> Reservation -> ContextMap k a -> Bool
isActiveReservation key reservation =
  maybe False matchesPending . Map.lookup key . contextEntries
  where
    matchesPending (ContextPending reservation') = reservation' == reservation
    matchesPending (ContextReady _) = False
