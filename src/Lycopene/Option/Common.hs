module Lycopene.Option.Common
        ( CommonOption(..)
        , commonOption
        ) where

import           Options.Applicative
import           Data.Monoid ((<>))

data CommonOption = CommonOption
                   { verbose :: Bool
                   } deriving (Show)

commonOption :: Parser CommonOption
commonOption = CommonOption <$> verboseP where
  verboseP = switch
          (  short 'v'
          <> long "verbose"
          <> help "Set to print verbosely")
