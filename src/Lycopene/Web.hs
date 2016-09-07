module Lycopene.Web where

import           Servant (serve)
import qualified Network.Wai.Handler.Warp as Warp
import           Lycopene.Environment (dataPath)
import           Lycopene.Database (connect)
import           Lycopene.Web.Api (api, server)

type PortNum = Int

startServer :: PortNum -> IO ()
startServer portNum = do
  d <- dataPath
  putStrLn $ "Listening on port " ++ (show portNum) ++ " with data " ++ d
  connection <- connect d
  Warp.run portNum $ serve api (server connection)

