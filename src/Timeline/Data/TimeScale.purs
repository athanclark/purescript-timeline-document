module Timeline.Data.TimeScale where

import Timeline.Time.MaybeLimit (MaybeLimit)
import Prelude
import Data.Tuple.Nested (type (/\), tuple4, uncurry4)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

-- | Parameters for defining how time is represented spatially and numerically
newtype TimeScale index
  = TimeScale
  { limit :: MaybeLimit index
  -- TODO human <-> presented interpolation
  -- non-essential
  , name :: String
  , description :: String
  , units :: String
  }

derive instance genericTimeScale :: Generic (TimeScale index) _

derive newtype instance eqTimeScale :: Eq index => Eq (TimeScale index)

derive newtype instance ordTimeScale :: Ord index => Ord (TimeScale index)

instance functorTimeScale :: Functor TimeScale where
  map f (TimeScale x) = TimeScale x { limit = map f x.limit }

instance foldableTimeScale :: Foldable TimeScale where
  foldr f acc (TimeScale x) = foldr f acc x.limit
  foldl f acc (TimeScale x) = foldl f acc x.limit
  foldMap f (TimeScale x) = foldMap f x.limit

instance traversableTimeScale :: Traversable TimeScale where
  traverse f (TimeScale x) = (\limit -> TimeScale x { limit = limit }) <$> traverse f x.limit
  sequence (TimeScale x) = (\limit -> TimeScale x { limit = limit }) <$> sequence x.limit

instance showTimeScale :: Show index => Show (TimeScale index) where
  show = genericShow

-- derive newtype instance encodeJsonTimeScale :: EncodeJson index => EncodeJson (TimeScale index)
instance encodeJsonTimeScale :: EncodeJson index => EncodeJson (TimeScale index) where
  encodeJson (TimeScale { limit, name, description, units }) =
    "limit" := limit
      ~> "name"
      := name
      ~> "description"
      := description
      ~> "units"
      := units
      ~> jsonEmptyObject

-- derive newtype instance decodeJsonTimeScale :: DecodeJson index => DecodeJson (TimeScale index)
instance decodeJsonTimeScale :: DecodeJson index => DecodeJson (TimeScale index) where
  decodeJson json = do
    o <- decodeJson json
    limit <- o .: "limit"
    name <- o .: "name"
    description <- o .: "description"
    units <- o .: "units"
    pure (TimeScale { limit, name, description, units })

instance encodeArrayBufferTimeScale :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeScale index) where
  putArrayBuffer b o (TimeScale { limit, name, description, units }) = putArrayBuffer b o (tuple4 limit name description units)

instance decodeArrayBufferTimeScale :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeScale index) where
  readArrayBuffer b o =
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> TimeScale index
      go = uncurry4 \limit name description units -> TimeScale { limit, name, description, units }
    in
      map go <$> readArrayBuffer b o

instance dynamicByteLengthTimeScale :: DynamicByteLength index => DynamicByteLength (TimeScale index) where
  byteLength (TimeScale { limit, name, description, units }) = byteLength (tuple4 limit name description units)

instance arbitraryTimeScale :: Arbitrary index => Arbitrary (TimeScale index) where
  arbitrary = do
    limit <- arbitrary
    name <- genString
    description <- genString
    units <- genString
    pure (TimeScale { limit, name, description, units })
