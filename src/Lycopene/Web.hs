module Lycopene.Web where

import           Servant (serve)
import qualified Network.Wai.Handler.Warp as Warp
import           Lycopene.Application (AppEngine)
import           Lycopene.Web.Api (api, server)

type PortNum = Int

startServer :: PortNum -> FilePath -> AppEngine -> IO ()
startServer portNum docRoot engine = do
  putStrLn $ "Listening on port " ++ (show portNum)
  Warp.run portNum $ serve api (server docRoot engine)

