module Lycopene.Option.Version (version) where


import Control.Applicative
import Options.Applicative

import Lycopene.Option.Command

data Version = Version
             { majorVersion :: Int
             , minorVersion :: Int
             , patchVersion :: Int
             } deriving (Eq)

version :: ParserInfo LycoSubcommand
version = info versionP (progDesc "Print appilcation version infomation")

versionP :: Parser LycoSubcommand
versionP = pure $ LycoSubcommand runVersion

runVersion c = do { putStrLn "version is 0.0.1" }

