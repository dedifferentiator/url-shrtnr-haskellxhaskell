{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Text
import Models
import Servant
import Servant.Auth as SA
import Servant.Auth.Server

type RequiredParam = QueryParam' '[Required, Strict]

-- TODO: JWT authentication
type Api auths =
  Signup
    :<|> Signin
    :<|> WithAuth auths :> Signout
    :<|> WithAuth auths :> Shorten
    :<|> WithAuth auths :> ListUrls
    :<|> WithAuth auths :> DeleteAlias
    :<|> RedirectAlias

type WithAuth auths =
  Auth auths User

type Signup =
  "users" :> "signup"
    :> RequiredParam "email" Email
    :> RequiredParam "password" Password
    :> Post '[JSON] NoContent

type Signin =
  "users" :> "signin"
    :> RequiredParam "email" Email
    :> RequiredParam "password" Password
    :> Verb
         'POST
         204
         '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             NoContent
         )

type Signout =
  "users" :> "signout"
    :> Post '[JSON] NoContent

type Shorten =
  "urls" :> "shorten"
    :> RequiredParam "url" Text
    :> QueryParam "alias" Text
    :> Post '[JSON] Text

type ListUrls =
  "urls"
    :> Get '[JSON] [Text]

type DeleteAlias =
  "urls"
    :> RequiredParam "alias" Text
    :> Delete '[JSON] NoContent

type RedirectAlias =
  "r"
    :> Capture "alias" Text
    :> Verb 'GET 303 '[PlainText] (Redirect Text)

type Redirect = Headers '[Header "Location" Text]
