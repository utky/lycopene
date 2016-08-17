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
