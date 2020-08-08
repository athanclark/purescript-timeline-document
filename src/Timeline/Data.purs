module Timeline.Data
  ( TimeSpan(..)
  , EventOrTimeSpan(..)
  , Timeline(..)
  , TimeSpace(..)
  , TimeSpaceDecided(..)
  , getIdTimeSpaceDecided
  , module Timeline.Data.Event
  , module Timeline.Data.TimeScale
  ) where

import Timeline.Time.Span
  ( Span
  , encodeJsonSpan
  , decodeJsonSpan
  , putArrayBufferSpan
  , readArrayBufferSpan
  , byteLengthSpan
  , genSpan
  )
import Timeline.Data.Event (Event(..))
import Timeline.Data.TimeScale (TimeScale(..))
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (unsafeMap, unsafeTraverse, unsafeSequence) as UniqueArray
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested
  ( type (/\)
  , tuple4
  , uncurry4
  , tuple6
  , uncurry6
  , get1
  , get2
  , get3
  , get4
  )
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Data.ArrayBuffer.Class.Types (Int8(..), Float64BE(..))
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf, sized, resize)
import Test.QuickCheck.UTF8String (genString)
import Type.Proxy (Proxy(..))

-- ------------------ TimeSpace
-- | A space where all time-definable values are stored; timelines, events, and time spans. Also defines how time
-- | is represented spatially; a time-scale.
newtype TimeSpace index
  = TimeSpace
  { timeScale :: TimeScale index
  , timelines :: UniqueArray (Timeline index)
  , siblings :: UniqueArray (EventOrTimeSpan index)
  -- non-essential
  , title :: String
  , description :: String
  , id :: TimeSpaceID
  -- TODO markers, metrics & graduation
  }

derive instance genericTimeSpace :: Generic (TimeSpace index) _

instance functorTimeSpace :: Functor TimeSpace where
  map f (TimeSpace x) =
    TimeSpace
      x
        { timelines = UniqueArray.unsafeMap (map f) x.timelines
        , siblings = UniqueArray.unsafeMap (map f) x.siblings
        , timeScale = map f x.timeScale
        }

instance foldableTimeSpace :: Foldable TimeSpace where
  foldr f acc (TimeSpace x) =
    let
      siblingsAcc = foldr (\eOrTs acc' -> foldr f acc' eOrTs) acc x.siblings

      timelinesAcc = foldr (\t acc' -> foldr f acc' t) siblingsAcc x.timelines
    in
      foldr f timelinesAcc x.timeScale
  foldl f acc (TimeSpace x) =
    let
      timeScaleAcc = foldl f acc x.timeScale

      timelinesAcc = foldl (\acc' t -> foldl f acc' t) timeScaleAcc x.timelines
    in
      foldl (\acc' eOrTs -> foldl f acc' eOrTs) timelinesAcc x.siblings
  foldMap f (TimeSpace x) =
    foldMap f x.timeScale
      <> foldMap (\t -> foldMap f t) x.timelines
      <> foldMap (\eOrTs -> foldMap f eOrTs) x.siblings

instance traversableTimeSpace :: Traversable TimeSpace where
  traverse f (TimeSpace x) =
    let
      go timeScale timelines siblings =
        TimeSpace
          x
            { timeScale = timeScale
            , timelines = timelines
            , siblings = siblings
            }
    in
      go <$> traverse f x.timeScale
        <*> UniqueArray.unsafeTraverse (\t -> traverse f t) x.timelines
        <*> UniqueArray.unsafeTraverse (\eOrTs -> traverse f eOrTs) x.siblings
  sequence (TimeSpace x) =
    let
      go timeScale timelines siblings =
        TimeSpace
          x
            { timeScale = timeScale
            , timelines = timelines
            , siblings = siblings
            }
    in
      go <$> sequence x.timeScale
        <*> UniqueArray.unsafeSequence (UniqueArray.unsafeMap sequence x.timelines)
        <*> UniqueArray.unsafeSequence (UniqueArray.unsafeMap sequence x.siblings)

instance eqTimeSpace :: Ord index => Eq (TimeSpace index) where
  eq (TimeSpace x) (TimeSpace y) =
    x.timeScale == y.timeScale
      && x.timelines
      == y.timelines
      && x.siblings
      == y.siblings
      && x.title
      == y.title
      && x.description
      == y.description
      && x.id
      == y.id

instance showTimeSpace :: Show index => Show (TimeSpace index) where
  show (TimeSpace x) =
    "(TimeSpace {timeScale: " <> show x.timeScale
      <> ", timelines: "
      <> show x.timelines
      <> ", siblings: "
      <> show x.siblings
      <> ", title: "
      <> show x.title
      <> ", description: "
      <> show x.description
      <> ", id: "
      <> show x.id
      <> "})"

instance encodeJsonTimeSpace :: EncodeJson index => EncodeJson (TimeSpace index) where
  encodeJson (TimeSpace { timeScale, timelines, siblings, title, description, id }) =
    "timeScale" := timeScale
      ~> "timelines"
      := timelines
      ~> "siblings"
      := siblings
      ~> "title"
      := title
      ~> "description"
      := description
      ~> "id"
      := id
      ~> jsonEmptyObject

instance decodeJsonTimeSpace :: (DecodeJson index, Ord index, Show index) => DecodeJson (TimeSpace index) where
  decodeJson json = do
    o <- decodeJson json
    timeScale <- o .: "timeScale"
    timelines <- o .: "timelines"
    siblings <- o .: "siblings"
    title <- o .: "title"
    description <- o .: "description"
    id <- o .: "id"
    pure
      $ TimeSpace
          { timeScale, timelines, siblings, title, description, id }

instance encodeArrayBufferTimeSpace :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpace index) where
  putArrayBuffer b o (TimeSpace { timeScale, timelines, siblings, title, description, id }) = putArrayBuffer b o (tuple6 timeScale timelines siblings title description id)

instance decodeArrayBufferTimeSpace :: (DecodeArrayBuffer index, DynamicByteLength index, Ord index, Show index) => DecodeArrayBuffer (TimeSpace index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> TimeSpace index
      go =
        uncurry6 \timeScale timelines siblings title description id ->
          TimeSpace { timeScale, timelines, siblings, title, description, id }
    map go <$> readArrayBuffer b o

instance dynamicByteLengthTimeSpace :: DynamicByteLength index => DynamicByteLength (TimeSpace index) where
  byteLength (TimeSpace { timeScale, timelines, siblings, title, description, id }) = byteLength (tuple6 timeScale timelines siblings title description id)

instance arbitraryTimeSpace :: (Arbitrary index, Ord index) => Arbitrary (TimeSpace index) where
  arbitrary = do
    timeScale <- arbitrary
    timelines <- sized \s -> resize (s `div` 4) arbitrary
    siblings <- sized \s -> resize (s `div` 4) arbitrary
    title <- genString
    description <- genString
    id <- arbitrary
    pure (TimeSpace { timeScale, timelines, siblings, title, description, id })

-- | All possible Human Time Indicies, with their instances assumed existing
data TimeSpaceDecided
  = TimeSpaceNumber (TimeSpace Number)

derive instance genericTimeSpaceDecided :: Generic TimeSpaceDecided _

instance eqTimeSpaceDecided :: Eq TimeSpaceDecided where
  eq x' y' = case Tuple x' y' of
    Tuple (TimeSpaceNumber x) (TimeSpaceNumber y) -> x == y
    _ -> false

-- instance ordTimeSpaceDecided :: Ord TimeSpaceDecided where
--   compare = genericCompare
instance showTimeSpaceDecided :: Show TimeSpaceDecided where
  show x = case x of
    TimeSpaceNumber y -> "(TimeSpaceNumber " <> show y <> ")"

instance encodeJsonTimeSpaceDecided :: EncodeJson TimeSpaceDecided where
  encodeJson x = case x of
    TimeSpaceNumber y -> "number" := y ~> jsonEmptyObject

instance decodeJsonTimeSpaceDecided :: DecodeJson TimeSpaceDecided where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = TimeSpaceNumber <$> o .: "number"
    decodeNumber

instance encodeArrayBufferTimeSpaceDecided :: EncodeArrayBuffer TimeSpaceDecided where
  putArrayBuffer b o x = case x of
    TimeSpaceNumber y -> do
      mW <- putArrayBuffer b o (Int8 0)
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBuffer b (o + w) (map Float64BE y)
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferTimeSpaceDecided :: DecodeArrayBuffer TimeSpaceDecided where
  readArrayBuffer b o = do
    mFlag <- readArrayBuffer b o
    case mFlag of
      Nothing -> pure Nothing
      Just (Int8 f)
        | f == 0 -> do
          mX <- readArrayBuffer b (o + 1)
          case mX of
            Nothing -> pure Nothing
            Just x -> pure $ Just $ TimeSpaceNumber $ map (\(Float64BE y) -> y) x
        | otherwise -> pure Nothing

instance dynamicByteLengthTimeSpaceDecided :: DynamicByteLength TimeSpaceDecided where
  byteLength x =
    map (_ + 1)
      $ case x of
          TimeSpaceNumber y -> byteLength (map Float64BE y)

instance arbitraryTimeSpaceDecided :: Arbitrary TimeSpaceDecided where
  arbitrary =
    oneOf
      $ NonEmpty
          (TimeSpaceNumber <$> arbitrary)
          []

getIdTimeSpaceDecided :: TimeSpaceDecided -> TimeSpaceID
getIdTimeSpaceDecided x = case x of
  TimeSpaceNumber (TimeSpace { id }) -> id

-- ------------------ Timeline
-- | Types of children in a Timeline
newtype EventOrTimeSpan index
  = EventOrTimeSpan (Either (Event index) (TimeSpan index))

derive instance genericEventOrTimeSpanData :: Generic index index' => Generic (EventOrTimeSpan index) _

instance eqEventOrTimeSpanData :: Eq index => Eq (EventOrTimeSpan index) where
  eq (EventOrTimeSpan x) (EventOrTimeSpan y) = case Tuple x y of
    Tuple (Left x') (Left y') -> x' == y'
    Tuple (Right x') (Right y') -> x' == y'
    _ -> false

instance showEventOrTimeSpanData :: Show index => Show (EventOrTimeSpan index) where
  show (EventOrTimeSpan x) =
    let
      content = case x of
        Left y -> show y
        Right y -> show y
    in
      "(EventOrTimeSpan " <> content <> ")"

instance functorEventOrTimeSpanData :: Functor EventOrTimeSpan where
  map f (EventOrTimeSpan x) =
    EventOrTimeSpan
      $ case x of
          Left y -> Left (map f y)
          Right y -> Right (map f y)

instance foldableEventOrTimeSpanData :: Foldable EventOrTimeSpan where
  foldr f acc (EventOrTimeSpan x) = case x of
    Left y -> foldr f acc y
    Right y -> foldr f acc y
  foldl f acc (EventOrTimeSpan x) = case x of
    Left y -> foldl f acc y
    Right y -> foldl f acc y
  foldMap f (EventOrTimeSpan x) = case x of
    Left y -> foldMap f y
    Right y -> foldMap f y

instance traversableEventOrTimeSpanData :: Traversable EventOrTimeSpan where
  traverse f (EventOrTimeSpan x) = case x of
    Left y -> EventOrTimeSpan <<< Left <$> traverse f y
    Right y -> EventOrTimeSpan <<< Right <$> traverse f y
  sequence (EventOrTimeSpan x) = case x of
    Left y -> EventOrTimeSpan <<< Left <$> sequence y
    Right y -> EventOrTimeSpan <<< Right <$> sequence y

instance encodeJsonEventOrTimeSpanData :: EncodeJson index => EncodeJson (EventOrTimeSpan index) where
  encodeJson (EventOrTimeSpan x) = case x of
    Left y -> "event" := y ~> jsonEmptyObject
    Right y -> "timeSpan" := y ~> jsonEmptyObject

instance decodeJsonEventOrTimeSpanData :: DecodeJson index => DecodeJson (EventOrTimeSpan index) where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeEvent = Left <$> o .: "event"

      decodeTimeSpan = Right <$> o .: "timeSpan"
    EventOrTimeSpan <$> (decodeEvent <|> decodeTimeSpan)

derive newtype instance encodeArrayBufferEventOrTimeSpanData :: EncodeArrayBuffer index => EncodeArrayBuffer (EventOrTimeSpan index)

derive newtype instance decodeArrayBufferEventOrTimeSpanData :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (EventOrTimeSpan index)

derive newtype instance dynamicByteLengthEventOrTimeSpanData :: DynamicByteLength index => DynamicByteLength (EventOrTimeSpan index)

derive newtype instance arbitraryEventOrTimeSpanData :: Arbitrary index => Arbitrary (EventOrTimeSpan index)

-- | A set of Timeline children - events and timespans
newtype Timeline index
  = Timeline
  { children :: UniqueArray (EventOrTimeSpan index) -- TODO optional auxillary sorting data
  -- non-essential
  , name :: String
  , description :: String
  -- TODO color
  , id :: TimelineID
  }

derive instance genericTimeline :: Generic (Timeline index) _

instance functorTimeline :: Functor Timeline where
  map f (Timeline x) = Timeline x { children = UniqueArray.unsafeMap (map f) x.children }

instance foldableTimeline :: Foldable Timeline where
  foldr f acc (Timeline x) = foldr (\eOrTs acc' -> foldr f acc' eOrTs) acc x.children
  foldl f acc (Timeline x) = foldl (\acc' eOrTs -> foldl f acc' eOrTs) acc x.children
  foldMap f (Timeline x) = foldMap (\eOrTs -> foldMap f eOrTs) x.children

instance traversableTimeline :: Traversable Timeline where
  traverse f (Timeline x) =
    (\children -> Timeline x { children = children })
      <$> UniqueArray.unsafeTraverse (\eOrTs -> traverse f eOrTs) x.children
  sequence (Timeline x) =
    (\children -> Timeline x { children = children })
      <$> UniqueArray.unsafeSequence (UniqueArray.unsafeMap sequence x.children)

instance eqTimeline :: Ord index => Eq (Timeline index) where
  eq (Timeline x) (Timeline y) =
    x.children == y.children
      && x.name
      == y.name
      && x.description
      == y.description
      && x.id
      == y.id

instance showTimeline :: Show index => Show (Timeline index) where
  show (Timeline { children, name, description, id }) =
    "(Timeline {children: " <> show children
      <> ", name: "
      <> show name
      <> ", description: "
      <> show description
      <> ", id: "
      <> show id
      <> "})"

instance encodeJsonTimeline :: EncodeJson index => EncodeJson (Timeline index) where
  encodeJson (Timeline { children, name, description, id }) =
    "children" := children
      ~> "name"
      := name
      ~> "description"
      := description
      ~> "id"
      := id
      ~> jsonEmptyObject

instance decodeJsonTimeline :: (DecodeJson index, Ord index, Show index) => DecodeJson (Timeline index) where
  decodeJson json = do
    o <- decodeJson json
    children <- o .: "children"
    name <- o .: "name"
    description <- o .: "description"
    id <- o .: "id"
    pure (Timeline { children, name, description, id })

instance encodeArrayBufferTimeline :: EncodeArrayBuffer index => EncodeArrayBuffer (Timeline index) where
  putArrayBuffer b o (Timeline { children, name, description, id }) = putArrayBuffer b o (tuple4 children name description id)

instance decodeArrayBufferTimeline :: (DecodeArrayBuffer index, DynamicByteLength index, Ord index, Show index) => DecodeArrayBuffer (Timeline index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> Timeline index
      go =
        uncurry4 \children name description id ->
          Timeline { children, name, description, id }
    map go <$> readArrayBuffer b o

instance dynamicByteLengthTimeline :: DynamicByteLength index => DynamicByteLength (Timeline index) where
  byteLength (Timeline { children, name, description, id }) = byteLength (tuple4 children name description id)

instance arbitraryTimeline :: (Arbitrary index, Ord index) => Arbitrary (Timeline index) where
  arbitrary = do
    children <-
      sized \size ->
        resize (size `div` 2) arbitrary
    name <- genString
    description <- genString
    id <- arbitrary
    pure (Timeline { children, name, description, id })

-- ------------------ TimeSpan
-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
newtype TimeSpan index
  = TimeSpan
  { timeSpace :: Maybe TimeSpaceDecided
  -- non-essential
  , name :: String
  , description :: String
  -- TODO color
  , id :: TimeSpanID
  , span :: Span index
  }

derive instance genericTimeSpan :: Generic index index' => Generic (TimeSpan index) _

instance functorTimeSpan :: Functor TimeSpan where
  map f (TimeSpan x) = TimeSpan x { span = { start: f x.span.start, stop: f x.span.stop } }

instance foldableTimeSpan :: Foldable TimeSpan where
  foldr f acc (TimeSpan x) = f x.span.start (f x.span.stop acc)
  foldl f acc (TimeSpan x) = f (f acc x.span.start) x.span.stop
  foldMap f (TimeSpan x) = f x.span.start <> f x.span.stop

instance traversableTimeSpan :: Traversable TimeSpan where
  traverse f (TimeSpan x) =
    (\start stop -> TimeSpan x { span = { start, stop } })
      <$> f x.span.start
      <*> f x.span.stop
  sequence (TimeSpan x) =
    (\start stop -> TimeSpan x { span = { start, stop } })
      <$> x.span.start
      <*> x.span.stop

instance eqTimeSpan :: Eq index => Eq (TimeSpan index) where
  eq (TimeSpan x) (TimeSpan y) =
    x.timeSpace == y.timeSpace
      && x.name
      == y.name
      && x.description
      == y.description
      && x.id
      == y.id
      && x.span
      == y.span

instance showTimeSpan :: Show index => Show (TimeSpan index) where
  show (TimeSpan x) =
    "TimeSpan {timeSpace: " <> show x.timeSpace
      <> ", name: "
      <> show x.name
      <> ", description: "
      <> show x.description
      <> ", id: "
      <> show x.id
      <> ", span: "
      <> show x.span
      <> "}"

instance encodeJsonTimeSpan :: EncodeJson index => EncodeJson (TimeSpan index) where
  encodeJson (TimeSpan { timeSpace, name, description, id, span }) =
    "timeSpace" := timeSpace
      ~> "name"
      := name
      ~> "description"
      := description
      ~> "id"
      := id
      ~> "span"
      := encodeJsonSpan (Proxy :: Proxy index) span
      ~> jsonEmptyObject

instance decodeJsonTimeSpan :: DecodeJson index => DecodeJson (TimeSpan index) where
  decodeJson json = do
    o <- decodeJson json
    timeSpace <- o .: "timeSpace"
    name <- o .: "name"
    description <- o .: "description"
    id <- o .: "id"
    span <- o .: "span" >>= decodeJsonSpan (Proxy :: Proxy index)
    pure $ TimeSpan { timeSpace, name, description, id, span }

instance encodeArrayBufferTimeSpan :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpan index) where
  putArrayBuffer b o (TimeSpan { timeSpace, name, description, id, span }) = do
    mW <- putArrayBuffer b o (tuple4 timeSpace name description id)
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        mW' <- putArrayBufferSpan (Proxy :: Proxy index) b (o + w) span
        case mW' of
          Nothing -> pure (Just w)
          Just w' -> pure (Just (w + w'))

instance decodeArrayBufferTimeSpan :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeSpan index) where
  readArrayBuffer b o = do
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just (xs :: _ /\ _ /\ _ /\ _ /\ Unit) -> do
        l <- byteLength xs
        mSpan <- readArrayBufferSpan (Proxy :: Proxy index) b (o + l)
        pure
          $ case mSpan of
              Nothing -> Nothing
              Just span ->
                Just
                  $ TimeSpan
                      { timeSpace: get1 xs
                      , name: get2 xs
                      , description: get3 xs
                      , id: get4 xs
                      , span
                      }

instance dynamicByteLengthTimeSpan :: DynamicByteLength index => DynamicByteLength (TimeSpan index) where
  byteLength (TimeSpan { timeSpace, name, description, id, span }) = do
    l <- byteLengthSpan (Proxy :: Proxy index) span
    l' <- byteLength (tuple4 timeSpace name description id)
    pure (l + l')

instance arbitraryTimeSpan :: Arbitrary index => Arbitrary (TimeSpan index) where
  arbitrary = do
    timeSpace <-
      let
        subtree = sized \s -> resize (s `div` 4) arbitrary
      in
        oneOf (NonEmpty (pure Nothing) [ Just <$> subtree ])
    name <- genString
    description <- genString
    id <- arbitrary
    span <- genSpan (Proxy :: Proxy index)
    pure (TimeSpan { timeSpace, name, description, id, span })
