{-# LANGUAGE DataKinds #-}
module Lycopene.Core.Stage where

-- | Life-cycle of stored entity
data Stage
    = Transient
    | Persistent
