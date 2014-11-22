{- | Dont use this module
-}
module Lycopene.Core.Project
    ( Project(..)
    ) where

data Project = Project
             { projectName :: String
             , projectDescription :: String
             }
