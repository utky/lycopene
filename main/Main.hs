module Main
        ( main
        ) where

import Options.Applicative as O
import Lycopene.Option
import Lycopene.Process as Ps
import System.Exit 
import Pipes as P

main :: IO ()
main = parseLycoCommand >>= runCommand >>= exitWith . mapExit where
  runCommand :: LycoCommand -> IO Result
  runCommand = P.runEffect . Ps.runProcess . Ps.processCommand
  parseLycoCommand = O.execParser lycoParser
  mapExit Ps.Success = ExitSuccess
  mapExit _       = ExitFailure 1
