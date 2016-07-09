module Main
        ( main
        ) where

-- import           Lycopene
import           System.Environment
import           System.FilePath
import           System.Directory (getHomeDirectory, getCurrentDirectory)


main :: IO ()
main = putStrLn "Hello world"
-- main = let conf home cd = defaultConfiguration
--                         { lycoHome = home </> ".lyco"
--                         , datapath = home </> ".lyco" </> "issues.db"
--                         , projectConf = cd </> ".lyco.conf"
--                         }
--        in do
--          args <- getArgs
--          home <- getHomeDirectory
--          cd   <- getCurrentDirectory
--          lycopene args (conf home cd)
