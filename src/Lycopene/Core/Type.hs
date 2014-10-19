module Lycopene.Core.Type () where

type Key = Integer
type Datetime = Integer

data Query a = Single a | Collection a
data Command a = Update a | Delete a | Insert a

class Entity a where
    key :: Eq b => a -> b
    save :: a -> Command a
    delete :: a -> Command a
    get :: a -> Query a

data Timestamp = Timestamp
               { createdOn :: Datetime
               , updatedOn :: Datetime
               }
