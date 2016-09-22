module Lycopene.Option.Parser where

import           Options.Applicative
import           Lycopene.Option.Common
import           Lycopene.Option.Command


type Arguments = [String]

lycoParser :: ParserInfo LycoCommand
lycoParser =
  info
    (helper <*> commandParser)
    (progDesc "tool belt for personal task management.")


execParserWithArgs :: ParserInfo a -> Arguments -> IO a
execParserWithArgs parser args =
  handleParseResult $ execParserPure (prefs idm) parser args


commandParser :: Parser LycoCommand
commandParser = LycoCommand <$> commonOption <*> subcommand

subcommand :: Parser Command
subcommand =
  subparser
    (  command "version" version
    <> command "configure" configure
    <> command "start" start
    )

---------------------------------------------------------------------

version :: ParserInfo Command
version =
  info
    (pure Version)
    (progDesc "Print appilcation version infomation.")


configure :: ParserInfo Command
configure =
  info
    (pure Configure)
    (progDesc "Initialize database if it doesn't exist.")

start :: ParserInfo Command
start =
  info
    (Start
      <$> (option auto
            (long "port"
            <> (short 'p')
            <> (metavar "PORT")
            <> (value 8080)
            <> showDefault
            <> (help "TCP port number in which server waits.")))
      <*> (strOption
            (long "directory"
            <> (short 'd')
            <> (metavar "DIR")
            <> (help "Directory of document root."))))
    (progDesc "Start web server.")
