module Lycopene.Core.Record where

import           Lycopene.Core.Scalar

data Record
    = Record
    { recordId :: !Identifier
    , recordStartOn :: !DateTime
    , recordEndOn :: !DateTime
    } deriving (Show)

instance Eq Record where
    x == y = (recordId x) == (recordId y)
