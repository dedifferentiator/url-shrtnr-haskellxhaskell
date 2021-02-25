{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers
  ( startApp,
    app,
  )
where

import Api
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server =
  signup
    :<|> signin
    :<|> signout
    :<|> shorten
    :<|> listUrls
    :<|> deleteAlias
    :<|> redirect
  where
    signup = undefined
    signin = undefined
    signout = undefined
    shorten = undefined
    listUrls = undefined
    deleteAlias = undefined

redirect :: Text -> Handler (Redirect Text)
redirect alias = do
  liftIO $ putStrLn $ "Redirected to " <> show alias
  pure $ addHeader ("https://" <> alias) "ok"
