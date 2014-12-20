-- | Includes functions providing command line I/O
module Lycopene.Option
            ( parseOptions
            , parserInfo
            , parseLycoCommand
            , module Lycopene.Option.Command
            ) where

import Control.Applicative
import Options.Applicative

import Lycopene.Option.Command
import Lycopene.Option.Version
import Lycopene.Option.Init


type Argument = String

parseLycoCommand :: IO LycoCommand
parseLycoCommand = execParser parserInfo

parseOptions :: [Argument] -> Maybe LycoCommand
parseOptions = getParseResult . execParserPure (prefs idm) parserInfo

parserInfo :: ParserInfo LycoCommand
parserInfo = info (helper <*> commandParser) ( progDesc "tool belt for personal task management." )

commandParser :: Parser LycoCommand
commandParser = LycoCommand <$> commonOption <*> subcommand

subcommand :: Parser LycoAction
subcommand = subparser
           ( command "version" version
           <> command "init" initDB
           )

