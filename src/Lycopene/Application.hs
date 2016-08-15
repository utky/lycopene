module Lycopene.Application where

import Lycopene.Core.Monad (LycopeneT, liftL)
import Lycopene.Configuration

application :: Lycopene a -> Reader Resources (Lycopene a)

runApplication :: Resources -> Lycopene a -> IO a
runApplication = runReader 
