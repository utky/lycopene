module Lycopene.Core.Type
    ( Command
    ) where

import qualified Data.Conduit as C
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.Class (Key)
import Control.Monad.Trans.Class
-- import Lycopene.Core.Monad (LycopeneT)

-- type LycoSource m o = C.Source (LycopeneT m) o
-- type LycoAction i m o = C.Conduit i (LycopeneT m) o

{- | Alias of domain operation
-}
type Command m i = C.Source (SqlPersistT m) (Key i)

