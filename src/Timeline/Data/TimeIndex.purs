module Timeline.Data.TimeIndex where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Float64BE(..))
import Data.Number (fromString) as Num

-- | Class that defines how this index is presented to humans, and how they can enter them in manually.
class HumanTimeIndex human interface | human -> interface where
  -- | Presenting the human form as an abbreviated string, for bubbles
  humanTimeIndexToString :: human -> String
  -- | Presenting the human form as an interface type for detailed adjustment
  humanTimeIndexToInterface :: human -> interface
  -- | Interpreting the human interfacing form
  humanTimeIndexFromInterface :: interface -> Either String human

-- FIXME what about integer positions, in some view box that's also an integer?
-- | How time-indexed data will be positioned spatially in a `TimeSpace`, according to some `TimeScale`.
class PositionedTimeIndex positioned where
  -- | Presenting
  positionedTimeIndexToNumber :: positioned -> Number

-- | A complete suite of time indicies, oriented around its most human and lossless form.
class
  ( HumanTimeIndex human interface
  , PositionedTimeIndex positioned
  , EncodeJson storableJson
  , DecodeJson storableJson
  , EncodeArrayBuffer storableArrayBuffer
  , DecodeArrayBuffer storableArrayBuffer
  ) <= TimeIndex human interface positioned storableJson storableArrayBuffer | human -> interface
  , human -> storableJson
  , human -> storableArrayBuffer
  , human -> positioned where
  humanToStorableJson :: human -> storableJson
  humanToStorableArrayBuffer :: human -> storableArrayBuffer
  storableJsonToHumanTimeIndex :: storableJson -> human
  storableArrayBufferToHumanTimeIndex :: storableArrayBuffer -> human
  humanToPresentableTimeIndex :: human -> positioned

-- ------------------ Instances
-- --------- Number
instance humanTimeIndexNumber :: HumanTimeIndex Number String where
  humanTimeIndexToString = show
  humanTimeIndexToInterface = show
  humanTimeIndexFromInterface s = case Num.fromString s of
    Nothing -> Left $ "Not a valid number string: " <> show s
    Just x -> Right x

instance positionedTimeIndexNumber :: PositionedTimeIndex Number where
  positionedTimeIndexToNumber = identity

instance timeIndexNumber :: TimeIndex Number String Number Number Float64BE where
  humanToStorableJson = identity
  storableJsonToHumanTimeIndex = identity
  humanToStorableArrayBuffer = Float64BE
  storableArrayBufferToHumanTimeIndex (Float64BE x) = x
  humanToPresentableTimeIndex = identity
