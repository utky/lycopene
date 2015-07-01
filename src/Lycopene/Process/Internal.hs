{-# LANGUAGE RankNTypes #-}
module Lycopene.Process.Internal
    ( Process'
    , module Lycopene.Process.Internal.Core
    ) where

import           Lycopene.Process.Internal.Core

-- | Simplified Process type with Domain monad
type Process' a = Process a IO ()
