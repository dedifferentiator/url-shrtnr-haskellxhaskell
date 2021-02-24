{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api,
  )
where

import Data.Text
import Models
import Servant

-- TODO: JWT authentication
type Api =
  Signup
    :<|> Signin
    :<|> Signout
    :<|> Shorten
    :<|> ListUrls
    :<|> DeleteAlias
    :<|> Redirect

type Signup =
  "users" :> "signup"
    :> QueryParam "email" Email
    :> QueryParam "password" Password
    :> PostNoContent

type Signin =
  "users" :> "signin"
    :> QueryParam "email" Email
    :> QueryParam "password" Password
    :> Post '[JSON] JWT

type Signout =
  "users" :> "signout"
    :> PostNoContent

type Shorten =
  "urls" :> "shorten"
    :> QueryParam "url" Url
    :> QueryParam "alias" Text
    :> Post '[JSON] Text

type ListUrls =
  "urls"
    :> Get '[JSON] [Text]

type DeleteAlias =
  "urls"
    :> QueryParam "alias" Text
    :> DeleteNoContent

-- REVIEW: Is there a type-safe way to indicate redirection?
type Redirect =
  "r"
    :> Capture "alias" Text
    :> Get '[JSON] NoContent
