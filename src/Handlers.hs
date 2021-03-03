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
import Control.Monad.Reader
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Models
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server as SAS

appToHandler :: AppConfig -> AppM a -> Handler a
appToHandler = flip runReaderT

mkApp ::
  Context '[SAS.CookieSettings, SAS.JWTSettings] ->
  CookieSettings ->
  JWTSettings ->
  AppConfig ->
  Application
mkApp cfg cs jwts appConf =
  serveWithContext api cfg $
    hoistServerWithContext
      api
      (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
      (appToHandler appConf)
      (server cs jwts)
  where
    api = Proxy :: Proxy (Api '[SAS.JWT])

startApp :: IO ()
startApp = do
  let port = 3001
  let appConf = AppConfig port
  -- TODO: we should persist the key
  myKey <- generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
  _ <- forkIO $ run port $ mkApp cfg cookieCfg jwtCfg appConf

  putStrLn $ "Started server on localhost:" <> show port
  putStrLn "Enter name and hash separated by a space for a new token"

  forever $ do
    xs <- words <$> getLine
    case xs of
      [name', hash'] -> do
        etoken <-
          makeJWT
            (User (Text.pack name') (Text.pack hash'))
            jwtCfg
            Nothing
        case etoken of
          Left e -> putStrLn $ "Error generating token:\t" ++ show e
          Right v -> putStrLn $ "New token:\t" ++ show v
      _ -> putStrLn "Expecting a name and password hash separated by spaces"

server :: CookieSettings -> JWTSettings -> ServerT (Api auths) AppM
server cs jwts =
  signup
    :<|> signin
    :<|> signout
    :<|> shorten
    :<|> listUrls
    :<|> deleteAlias
    :<|> redirect

signup :: Email -> Password -> AppM NoContent
signup email pass = undefined

signin ::
  Email ->
  Password ->
  AppM
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        NoContent
    )
signin email pass = undefined

signout :: SAS.AuthResult User -> AppM NoContent
signout (SAS.Authenticated user) = undefined
signout _ = throwError err401

shorten :: SAS.AuthResult User -> Text -> Maybe Text -> AppM Text
shorten (SAS.Authenticated user) link mAlias = undefined
shorten _ _ _ = throwError err401

listUrls :: SAS.AuthResult User -> AppM [Text]
listUrls (SAS.Authenticated user) = undefined
listUrls _ = throwError err401

deleteAlias :: SAS.AuthResult User -> Text -> AppM NoContent
deleteAlias (SAS.Authenticated user) alias = undefined
deleteAlias _ _ = throwError err401

redirect :: Text -> AppM (Headers '[Header "Location" Text] Text)
redirect alias = do
  liftIO $ putStrLn $ "Redirected to " <> show alias
  pure $ addHeader ("https://" <> alias) "ok"
