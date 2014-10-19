-- | Includes functions providing command line I/O
module Lycopene.Option
            ( parseOptions
            ) where

import Options.Applicative

type Argument = String

data LycoCommand = Version | Project deriving (Show, Eq)


parseOptions :: [Argument] -> Maybe LycoCommand
parseOptions = getParseResult . execParserPure (prefs idm) parserInfo

parserInfo :: ParserInfo LycoCommand
parserInfo = info commandParser ( progDesc "interactive shell for document development." )

commandParser :: Parser LycoCommand
commandParser = subparser
    ( 
          command "version" (info versionParser (progDesc "indicate appilcation version"))
      <>  command "project" (info projectParser (progDesc "initialize current directory"))
    )

versionParser :: Parser LycoCommand
versionParser = pure Version

projectParser :: Parser LycoCommand
projectParser = pure Project

