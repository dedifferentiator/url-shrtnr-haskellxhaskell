{-# LANGUAGE FlexibleInstances #-}

module AppM where

import qualified Crypto.BCrypt as BCrypt
import qualified Data.Text.Encoding as Encoding
import Control.Monad.Reader
import Servant (Handler)
import Typeclasses
import Storage
import Models

type AppM = ReaderT AppConfig Handler

instance Database AppM where
  getAllUsers = getAllUsersFs
  getAllAliases = getAllAliasesFs
  addUser = addUserFs
  lookupUser = lookupUserFs
  removeUser = removeUserFs
  addAlias = addAliasFs
  lookupAlias = lookupAliasFs
  removeAlias = removeAliasFs

instance Logger AppM where
  logInfo = liftIO . putStrLn
  logWarning = liftIO . putStrLn
  logError = liftIO . putStrLn

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
