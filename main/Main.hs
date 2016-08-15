module Main
        ( main
        ) where

-- import           Lycopene (Lycopene)
import           Lycopene.Option (execParserWithArgs, lycoParser)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  cmd <- execParserWithArgs lycoParser args
  putStrLn $ show cmd
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
