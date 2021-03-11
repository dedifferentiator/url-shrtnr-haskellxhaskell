{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( startApp,
  )
where

import Api
import Authentication
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Functor
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Models
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server as SAS
import Typeclasses
import Urls
import AppM

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
  let appConf = AppConfig port ""
  -- TODO: we should persist the key
  myKey <- generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
  run port $ mkApp cfg cookieCfg jwtCfg appConf

server :: CookieSettings -> JWTSettings -> ServerT (Api auths) AppM
server cs jwts =
  signup
    :<|> signin cs jwts
    :<|> signout cs
    :<|> shorten
    :<|> listUrls
    :<|> deleteAlias
    :<|> redirect

signup :: Email -> Password -> AppM NoContent
signup email password = do
  result <- registerUser email password
  case result of
    (Right _) -> pure NoContent
    (Left _) -> throwError err400

signin ::
  CookieSettings ->
  JWTSettings ->
  Email ->
  Password ->
  AppM
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        NoContent
    )
signin cookies jwts email pass = do
  valid <- signinCheck email pass
  if valid
    then do
      mApplyCookies <- liftIO $ acceptLogin cookies jwts (User email pass)
      case mApplyCookies of
        Nothing -> throwError err404
        Just applyCookies -> pure $ applyCookies NoContent
    else throwError err404

signout ::
  CookieSettings ->
  SAS.AuthResult User ->
  AppM
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        NoContent
    )
signout cookies (SAS.Authenticated _) = pure $ SAS.clearSession cookies NoContent
signout _ _ = throwError err401

shorten :: SAS.AuthResult User -> Text -> Maybe Text -> AppM Text
shorten (SAS.Authenticated user) link mAlias = undefined
shorten _ _ _ = throwError err401

listUrls :: SAS.AuthResult User -> AppM [Text]
listUrls (SAS.Authenticated user) = undefined
listUrls _ = throwError err401

deleteAlias :: SAS.AuthResult User -> Text -> AppM NoContent
deleteAlias (SAS.Authenticated user) alias = undefined
deleteAlias _ _ = throwError err401

redirect :: AliasName -> AppM (Headers '[Header "Location" Text] Text)
redirect alias = do
  mAlias <- redirectUser alias
  case mAlias of
    Just url -> pure $ addHeader url "ok"
    Nothing -> throwError err404
