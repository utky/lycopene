module Main
        ( main
        ) where

import Options.Applicative (execParser)
import Lycopene.Option as Op
import Lycopene.Process as Ps
import Lycopene.Resource
import Lycopene.Core (runLycopene)
import System.Exit 
import Pipes (runEffect)


-- build :: LycoCommand -> IO (Configure, Process)
-- runEffect . runProcess :: Process -> LycoepeneT Persist Result
-- flip . runLycopeneT $ Configure :: 

main :: IO ()
main = do
  let parseLycoCommand = execParser Op.lycoParser
      exitF = exitWith . mapExit
      runCommand = runEffect . Ps.runProcess . Ps.processCommand
      allocateConfig = allocate . configResource
  cmd <- parseLycoCommand
  eitherConfig <- allocateConfig cmd
  case eitherConfig of
    Right cfg -> runLycopene (runCommand cmd) cfg >>= exitF
    Left e    -> putStrLn "resource error" >> exitFailure


-- | TODO: vary exit code
mapExit :: Ps.Result -> ExitCode
mapExit Ps.Success = ExitSuccess
mapExit _       = ExitFailure 1

