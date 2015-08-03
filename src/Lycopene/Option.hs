-- | Includes functions providing command line I/O
module Lycopene.Option
            ( lycoParser
            , execParserWithArgs
            , module Lycopene.Option.Command
            , module Lycopene.Option.Common
            ) where

import Options.Applicative

import Lycopene.Option.Common
import Lycopene.Option.Command
import Lycopene.Option.Version
import Lycopene.Option.Configure
import Lycopene.Option.Init
import Lycopene.Option.Ls
import Lycopene.Option.Project
import Lycopene.Option.Sp
import Lycopene.Option.Hs
import Lycopene.Option.New
import Lycopene.Option.Run
import Lycopene.Option.Done
import Lycopene.Configuration
-- import Lycopene.Option.Project
--

-- コマンドとはユーザからの入力、設定と環境を受け取って
-- 計算の結果を標準出力に書き込む
--
-- Parsed Option
-- Env + common option -> Configuration
--

type Arguments = [String]

execParserWithArgs :: ParserInfo a -> Arguments -> IO a
execParserWithArgs parser args = handleParseResult $ execParserPure (prefs idm) parser args

lycoParser :: Configuration -> ParserInfo LycoCommand
lycoParser conf = info (helper <*> commandParser conf) ( progDesc "tool belt for personal task management." )

commandParser :: Configuration ->  Parser LycoCommand
commandParser conf = LycoCommand <$> commonOption <*> subcommand conf

subcommand :: Configuration -> Parser Command
subcommand conf = subparser
           (  command "version" version
           <> command "configure" configureDB
           <> command "init" (initProject conf)
           <> command "ls" listIssues
           <> command "new" newIssue
           <> command "pj" listProjects
           <> command "sp" listSprints
           <> command "hs" recordHistory
           <> command "run" runTimer
           <> command "done" doneIssue
           )
