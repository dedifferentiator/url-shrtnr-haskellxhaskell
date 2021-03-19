{-# LANGUAGE FlexibleContexts #-}

module Authentication (registerUser, signinCheck) where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString as ByteString
import Data.Functor
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Models
import Typeclasses

registerUser ::
  (Database m, Hasher m) =>
  Email ->
  Password ->
  m (Either AppError ())
registerUser email password = do
  mHash <- hashPassword password
  case mHash of
    Nothing -> pure (Left RegistrationError)
    Just hash -> do
      mAdded <- addUser (User email hash)
      case mAdded of
        Just _ -> pure (Right ())
        Nothing -> pure (Left RegistrationError)

signinCheck :: (Database m, Hasher m) => Email -> Password -> m Bool
signinCheck email pass = do
  mUser <- lookupUser email
  case mUser of
    Just (User _ uhash) -> validatePassword pass uhash
    Nothing -> pure False
