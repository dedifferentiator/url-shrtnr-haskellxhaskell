{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Typeclasses where

import Control.Monad.IO.Class
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Text.Encoding as Encoding
import Models

type family Key a where
  Key User = Email
  Key Alias = AliasName

class (Monad m) => Database m where
  getAllUsers :: m [User]
  getAllAliases :: m [Alias]

  addUser :: User -> m ()
  lookupUser :: Key User -> m (Maybe User)
  removeUser :: Key User -> m ()

  addAlias :: Alias -> m ()
  lookupAlias :: Key Alias -> m (Maybe Alias)
  removeAlias :: Key Alias -> m ()

class (Monad m) => Logger m where
  logInfo :: String -> m ()
  logWarning :: String -> m ()
  logError :: String -> m ()

class (Monad m) => Hasher m where
  hashPassword :: Password -> m (Maybe PasswordHash)
  validatePassword :: PasswordHash -> Password -> m Bool

instance Logger AppM where
  logInfo = liftIO . putStrLn
  logWarning = liftIO . putStrLn
  logError = liftIO . putStrLn

instance Database AppM where
  getAllUsers = undefined
  getAllAliases = undefined
  addUser = undefined
  lookupUser = undefined
  removeUser = undefined
  addAlias = undefined
  lookupAlias = undefined
  removeAlias = undefined

instance Hasher AppM where
  hashPassword pass = do
    let toBytecode = Encoding.encodeUtf8
        fromBytecode = Encoding.decodeUtf8

    mHash <-
      liftIO $
        BCrypt.hashPasswordUsingPolicy
          BCrypt.slowerBcryptHashingPolicy
          (toBytecode pass)

    pure $ fromBytecode <$> mHash

  validatePassword pass hash = do
    let toBytecode = Encoding.encodeUtf8
    pure $ BCrypt.validatePassword (toBytecode hash) (toBytecode pass)
