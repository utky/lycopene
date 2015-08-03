module Lycopene.Configuration 
    ( Configuration(..)
    , defaultConfiguration
    ) where

data Configuration = Configuration
                   { lycoHome :: FilePath
                   , datapath :: String
                   , projectConf :: FilePath
                   , pomodoroMinutes :: Int
                   , shortBreakMinutes :: Int
                   , longBreakMinutes :: Int
                   }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
                     { lycoHome = "."
                     , datapath = ":memory:"
                     , projectConf = ".lyco.conf"
                     , pomodoroMinutes = 25
                     , shortBreakMinutes = 5
                     , longBreakMinutes = 15
                     }
