module Lycopene.Application (Lycopene) where

import Control.Monad.Reader
import Lycopene.Configuration

data Lycopene = Sucess
               | Fail
               | Process Configuration

instance Show Lycopene where
    show Process c = "Process with configuration: " ++ (show c)
    show Success = "Success"
    show Fail = "Fail"


