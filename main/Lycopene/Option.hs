-- | Includes functions providing command line I/O
module Lycopene.Option
            ( lycoParser
            , module Lycopene.Option.Command
            , module Lycopene.Option.Common
            ) where

import Control.Applicative
import Options.Applicative

import Lycopene.Option.Common
import Lycopene.Option.Command
import Lycopene.Option.Version
import Lycopene.Option.Configure
import Lycopene.Option.Init
-- import Lycopene.Option.Project
--

-- コマンドとはユーザからの入力、設定と環境を受け取って
-- 計算の結果を標準出力に書き込む
--
-- Parsed Option
-- Env + common option -> Configuration
--


lycoParser :: ParserInfo LycoCommand
lycoParser = info (helper <*> commandParser) ( progDesc "tool belt for personal task management." )

commandParser :: Parser LycoCommand
commandParser = LycoCommand <$> commonOption <*> subcommand

subcommand :: Parser Command
subcommand = subparser
           ( command "version" version
           <> command "configure" configureDB
           <> command "init" initProject
           )
