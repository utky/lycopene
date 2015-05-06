module Lycopene.Core
      ( module Lycopene.Core.Monad
      , module Lycopene.Core.Database
      , runLycopene
      ) where

import          Lycopene.Core.Monad
import          Lycopene.Core.Database (Persist, runPersist, connect)
import          Lycopene.Configuration

runLycopene :: LycopeneT Persist a -> Configuration -> IO a
runLycopene t c = 
  let p = runLycopeneT t c
      runM = runPersist p
  in  connect c >>= runM

