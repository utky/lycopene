module Main
        ( main
        ) where

import           Lycopene
import           Lycopene.Process (ProcessEnv(..))
import qualified System.IO as IO
import           System.Exit
import           System.Environment
import           System.FilePath
import           System.Directory (getHomeDirectory)


main :: IO ()
main = let psenv     = ProcessEnv
                       { hOut = IO.stdout
                       , hErr = IO.stderr
                       , hIn  = IO.stdin
                       }
           conf home = Configuration
                       { lycoHome = home </> ".lyco"
                       , datapath = home </> ".lyco" </> "issues.db"
                       , targetProject = 0
                       }
       in  (,) <$> getArgs <*> getHomeDirectory >>= (\(args, home) -> lycopene args (conf home) psenv)
