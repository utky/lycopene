module Lycopene.Core.Scalar where

import           Data.Time (UTCTime, Day)
import           Data.UUID (UUID, toString)

type Description = Maybe String
type Name = String
type Date = Day
type DateTime = UTCTime
type Identifier = UUID

idStr :: Identifier -> String
idStr = toString
