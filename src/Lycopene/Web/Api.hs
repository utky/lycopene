{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Api where

import           Servant
import           Lycopene.Database (DataSource)

type LycopeneApi
  =    "ping" :> Get '[JSON] String

api :: Proxy LycopeneApi
api = Proxy

server :: DataSource -> Server LycopeneApi
server ds
  =    ping
  where
    ping = return "pong"

