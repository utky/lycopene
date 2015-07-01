{-# LANGUAGE RankNTypes #-}
module Lycopene.Process
    ( buildProcess
    , runProcess
    , Chunk
    , module Lycopene.Process.Internal
    ) where


import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..))
import           Lycopene.Process.Internal

import           Lycopene.Configuration (Configuration)
import           Lycopene.Print (Print)

import           Lycopene.Action
import           Lycopene.Process.Version   (version)
import           Lycopene.Process.Configure (configure)
import           Lycopene.Process.Init      (initialize)
import           Lycopene.Process.Ls        (listIssues)
import           Lycopene.Process.New       (newIssue)

import           System.FilePath ((</>))

processAction :: Action a -> Configuration -> Process' a
processAction action cfg = 
  do e <- liftIO . handleResult $ runAction action cfg
     case e of
       (Left err) -> fatal "やばいよー" -- FIXME
       (Right x)  -> yield x


process :: (Print a) => Action a -> Configuration -> Process' String
process action cfg = printer <-< processAction action cfg

type Chunk = Either String String

buildProcess :: LycoCommand -> Configuration -> Producer Chunk IO ()
buildProcess (LycoCommand commonOpt cmd) cfg = runWriterPs $ mkPs cmd cfg where
  mkPs Version   = process version
  mkPs Configure = process configure
  mkPs (Init mName mDesc path) = process (initialize mName mDesc path)
  mkPs _         = \_ -> fatal "No command specified"

runProcess :: Producer Chunk IO () -> ProcessEnv -> Effect IO ()
runProcess ps (ProcessEnv hout herr hin) =
                    let cOut = handleOut hout
                        cErr = handleOut herr
                        out  = choice cErr cOut
                    in  ps >-> out
