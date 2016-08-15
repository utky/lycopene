module Main
        ( main
        ) where

-- import           Lycopene (Lycopene)
import           Lycopene.Option (execParserWithArgs, lycoParser)
import           Lycopene.Command (runCommand)
import           Lycopene.Configuration (loadConfig)
import           Lycopene.Environment (configPath, initializeDirs)
import           System.IO (stderr, hPutStrLn)
import           System.Environment (getArgs)

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  cmd <- execParserWithArgs lycoParser args
  initializeDirs
  cfgE <- configPath >>= loadConfig
  case cfgE of
    (Left errMsg) -> putStrLnErr errMsg
    (Right cfg) -> runCommand cfg cmd
  -- result <- runApp config $ runCommand cmd
  -- handleResult result
  -- putStrLn "Hello world"

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
