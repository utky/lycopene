module Main
        ( main
        ) where

import           Lycopene.Option (parseLycoCommand)
import           Lycopene.Process (runProcess', processCommand)
import           Lycopene.Resource (configResource)
import           Lycopene.Core (runLycopene)
import           System.Exit 


main :: IO ()
main = do
  let runCommand = runProcess' . processCommand
  cmd <- parseLycoCommand
  cfg <- configResource cmd
  case eitherConfig of
    Right cfg -> runCommand cfg cmd >>= exitWith . mapExit
    Left e    -> putStrLn "resource error" >> exitFailure

-- | TODO: vary exit code
mapExit :: Ps.Result -> ExitCode
mapExit Ps.Success = ExitSuccess
mapExit _          = ExitFailure 1

