{-# LANGUAGE RankNTypes #-}
module Lycopene
    ( lycopene
    , module Lycopene.Configuration
    ) where

import           Lycopene.Configuration
import           Lycopene.Option
import           Lycopene.Process
import           Lycopene.Core
import           System.Exit

{- | Overview
 - [String] -> Dom a | a
 - [String] -> Command
 - route :: Command -> Action
 - execute :: Action -> Process
 - Dom a | a -> OutStream a
 -
 - OutStream a -> Environment -> IO ()
 -
 -
 -
 - run :: Process -> ???
 -
 -}

type Arguments = [String]

lycopene :: Arguments ->  Configuration -> ProcessEnv -> IO ()
lycopene args conf psenv = let cmd = execParserWithArgs lycoParser args
                               runProcess' = flip runProcess psenv
                               buildProcess' = flip buildProcess conf
                               -- execCmd = runEffect . runProcess' . buildProcess'
                           in cmd >>= runCommand conf



-- fetchResource :: Configuration -> Resource Context
-- fetchResource cfg = let conn = connection . connectSqlite3 $ datapath cfg
--                     in Context
--                        <$> return 0 -- FIXME
--                        <*> fmap (mkDataSource . ConnWrapper) conn

runExit :: Either LycoError a -> ExitCode
runExit (Left _)  = ExitFailure 1
runExit (Right x) = ExitSuccess
