module Lycopene.Application (Lycopene) where

import Control.Monad.Reader

import Lycopene.Core
import Lycopene.Configuration


data LycoApp i r = LycoApp
                 { getConfiguration :: Configuration
                 , service :: (Lycopene i IO r)
                 }

