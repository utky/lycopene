{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
module Lycopene.Core.Store
  ( Change
  , Volatile
  , Stored
  ) where

import           Lycopene.Core.Stage (Stage(..))

type Change a = a -> a
type Modification b = b -> b

-- | Operations for store.
data Store :: Stage -> * -> * where
  Add   :: Store 'Transient a -> Store 'Persistent a
  Update :: Change a -> Store 'Persistent a -> Store 'Persistent a
  Fetch  :: Store 'Persistent a
  Remove :: Store 'Persistent a -> Store 'Transient a

-- | 
type Volatile = Store 'Transient
-- |
type Stored = Store 'Persistent
