{-# LANGUAGE FlexibleInstances #-}
module Lycopene.Core.Scalar where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import           Data.Aeson.Types (typeMismatch, Value(..), Parser)
import           Data.Time (UTCTime(..), Day)
import           Data.UUID (UUID, toString)
import           Data.Time.Clock (utctDay)

type Description = Maybe String
type Name = String
type Date = Day
type DateTime = UTCTime
type Identifier = UUID

toDate :: DateTime -> Date
toDate = utctDay

toTime :: Date -> DateTime
toTime d = UTCTime d 0

idStr :: Identifier -> String
idStr = toString

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


