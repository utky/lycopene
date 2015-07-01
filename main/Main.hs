module Main
        ( main
        ) where

import           Lycopene
import           Lycopene.Process (ProcessEnv(..))
import qualified System.IO as IO
import           System.Exit
import           System.Environment


main :: IO ()
main = let psenv = ProcessEnv
                   { hOut = IO.stdin
                   , hErr = IO.stderr
                   , hIn  = IO.stdout
                   }
       in  getArgs >>= (\args -> lycopene args defaultConfiguration psenv)
