module Lycopene.Web where

import           Servant (serve)
import qualified Network.Wai.Handler.Warp as Warp
import           Lycopene.Application (AppEngine)
import           Lycopene.Web.Api (api, server)

type PortNum = Int

startServer :: PortNum -> AppEngine -> IO ()
startServer portNum engine = do
  putStrLn $ "Listening on port " ++ (show portNum)
  Warp.run portNum $ serve api (server engine)

