{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Api

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server = pure NoContent
