-- | Includes functions providing command line I/O
module Lycopene.Option
            ( parseOptions
            , module Lycopene.Option.Command
            ) where

import Control.Applicative
import Options.Applicative

import Lycopene.Option.Command
import Lycopene.Option.Version

{- | Hmmmmm,
 - How to think about 
 -}

type Argument = String

parseOptions :: [Argument] -> Maybe LycoCommand
parseOptions = getParseResult . execParserPure (prefs idm) parserInfo

parserInfo :: ParserInfo LycoCommand
parserInfo = info commandParser ( progDesc "interactive shell for document development." )

commandParser :: Parser LycoCommand
commandParser = LycoCommand <$> commonOption <*> subcommand

subcommand :: Parser LycoSubcommand
subcommand = subparser
           ( command "version" version
           )

