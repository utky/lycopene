{-# LANGUAGE FlexibleInstances #-}
module Lycopene.Core.Scalar where

import           Web.HttpApiData
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import           Data.Aeson.Types (typeMismatch, Value(..))
import           Data.Time (UTCTime(..), Day)
import           Data.UUID (UUID, toString, fromString)
import           Data.UUID.V4 (nextRandom)
import           Data.Time.Clock (utctDay)
import qualified Data.Text as T

type Description = Maybe String
type Name = String
type Date = Day
type DateTime = UTCTime
type Identifier = Id

newtype Id = Id { unId :: UUID } deriving (Show, Eq)

instance FromHttpApiData Id where
  parseQueryParam = fmap uuid . parseQueryParam

instance ToJSON Id where
  toJSON = String . T.pack . show . unId

instance FromJSON Id where
  parseJSON j@(String t) =
    case fromString (T.unpack t) of
      (Just u) -> return $ Id u
      Nothing  -> typeMismatch ("Invalid UUID format: " ++ (show t)) j
  parseJSON invalid = typeMismatch "UUID" invalid

uuid :: UUID -> Identifier
uuid = Id

nextId :: IO Id
nextId = fmap uuid nextRandom

toDate :: DateTime -> Date
toDate = utctDay

toTime :: Date -> DateTime
toTime d = UTCTime d 0

idStr :: Identifier -> String
idStr = toString . unId

class Numeric a where
  toInt :: a -> Integer
  parseInteger :: Integer -> Either String a

-- instance (Numeric a) => ToJSON a where
--   toJSON = toJSON . toInt
-- 
-- instance (Numeric a) => FromJSON a where
--   parseJSON j@(Number x) = 
--     let withInteger :: Integer -> Parser a
--         withInteger n =
--           case parseInteger n of
--             (Right y) -> return y
--             (Left s)  -> typeMismatch s j
--     in  parseJSON j >>= withInteger
--   parseJSON invalid = typeMismatch "Number" invalid


