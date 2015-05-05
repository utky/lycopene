module Main
        ( main
        ) where

import           Lycopene.Option (parseLycoCommand)
import qualified Lycopene.Process as P
import           Lycopene.Resource (configResource)
import           Lycopene.Core (runLycopene)
import           System.Exit 


main :: IO ()
main = do
  let runCommand a b = P.runProcess' $ P.processCommand a b
  cmd <- parseLycoCommand
  cfg <- configResource cmd
  runCommand cfg cmd >>= exitWith . mapExit

-- | TODO: vary exit code
mapExit :: P.Result -> ExitCode
mapExit P.Success = ExitSuccess
mapExit _         = ExitFailure 1

