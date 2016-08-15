module Lycopene.Action.Run where

import           Lycopene.Action
import qualified Lycopene.Core.Record as Record

import           Control.Monad (void)
import           Data.Time.LocalTime

record :: Integer -> LocalTime-> Action ()
record i t = domain $ void (Record.newRecord i t)
