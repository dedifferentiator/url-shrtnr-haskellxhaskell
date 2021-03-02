{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( startApp,
  )
where

import Api
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Models
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server as SAS

-- REVIEW: we can make some sort of ReaderT AppConfig IO a later here
-- data AppConfig = AppConfig
--   { appPort :: Int
--   }

startApp :: IO ()
startApp = do
  let port = 3001
  -- TODO: we should persist the key
  myKey <- generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (Api '[SAS.JWT])
  _ <- forkIO $ run port $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)

  putStrLn $ "Started server on localhost:" <> show port
  putStrLn "Enter name and hash separated by a space for a new token"

  forever $ do
    xs <- words <$> getLine
    case xs of
      [name', hash'] -> do
        etoken <- makeJWT (User (Text.pack name') (Text.pack hash')) jwtCfg Nothing
        case etoken of
          Left e -> putStrLn $ "Error generating token:\t" ++ show e
          Right v -> putStrLn $ "New token:\t" ++ show v
      _ -> putStrLn "Expecting a name and password hash separated by spaces"

server :: CookieSettings -> JWTSettings -> Server (Api auths)
server cs jwts =
  signup
    :<|> signin
    :<|> signout
    :<|> shorten
    :<|> listUrls
    :<|> deleteAlias
    :<|> redirect

signup :: Server Signup
signup = undefined

signin :: Server Signin
signin = undefined

signout :: SAS.AuthResult User -> Server Signout
signout = undefined

shorten :: SAS.AuthResult User -> Server Shorten
shorten = undefined

listUrls :: SAS.AuthResult User -> Server ListUrls
listUrls = undefined

deleteAlias :: SAS.AuthResult User -> Server DeleteAlias
deleteAlias = undefined

redirect :: Text -> Handler (Redirect Text)
redirect alias = do
  liftIO $ putStrLn $ "Redirected to " <> show alias
  pure $ addHeader ("https://" <> alias) "ok"
