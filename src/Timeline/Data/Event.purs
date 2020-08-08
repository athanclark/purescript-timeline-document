module Timeline.Data.Event where

import Timeline.ID.Event (EventID)
import Prelude
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), tuple4, uncurry4)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), decodeJson)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | A punctuated event in time
newtype Event index
  = Event
  { name :: String
  , description :: String
  -- TODO color
  , id :: EventID
  , time :: index
  }

derive instance genericEvent :: Generic index index' => Generic (Event index) _

derive newtype instance eqEvent :: Eq index => Eq (Event index)

derive newtype instance showEvent :: Show index => Show (Event index)

instance functorEvent :: Functor Event where
  map f (Event x) = Event x { time = f x.time }

instance foldableEvent :: Foldable Event where
  foldr f acc (Event x) = f x.time acc
  foldl f acc (Event x) = f acc x.time
  foldMap f (Event x) = f x.time

instance traversableEvent :: Traversable Event where
  traverse f (Event x) = (\y -> Event x { time = y }) <$> f x.time
  sequence (Event x) = (\y -> Event x { time = y }) <$> x.time

instance encodeJsonEvent :: EncodeJson index => EncodeJson (Event index) where
  encodeJson (Event { name, description, id, time }) =
    "name" := name
      ~> "description"
      := description
      ~> "id"
      := id
      ~> "time"
      := time
      ~> jsonEmptyObject

instance decodeJsonEvent :: DecodeJson index => DecodeJson (Event index) where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    id <- o .: "id"
    time <- o .: "time"
    pure $ Event { name, description, id, time }

instance encodeArrayBufferEvent :: EncodeArrayBuffer index => EncodeArrayBuffer (Event index) where
  putArrayBuffer b o (Event { name, description, id, time }) = putArrayBuffer b o (tuple4 name description id time)

instance decodeArrayBufferEvent :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (Event index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> Event index
      go =
        uncurry4 \name description id time ->
          Event { name, description, id, time }
    map go <$> readArrayBuffer b o

instance dynamicByteLengthEvent :: DynamicByteLength index => DynamicByteLength (Event index) where
  byteLength (Event { name, description, id, time }) = byteLength (tuple4 name description id time)

instance arbitraryEvent :: Arbitrary index => Arbitrary (Event index) where
  arbitrary = do
    name <- genString
    description <- genString
    id <- arbitrary
    time <- arbitrary
    pure (Event { name, description, id, time })
