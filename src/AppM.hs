{-# LANGUAGE FlexibleInstances #-}

module AppM where

import Control.Monad.Reader
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Models
import Servant (Handler)
import Storage
import qualified System.Random as Random
import Typeclasses

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
  logInfo = liftIO . putStrLn . ("[INFO] " <>)
  logWarning = liftIO . putStrLn . ("[WARN] " <>)
  logError = liftIO . putStrLn . ("[ERROR]" <>)

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

  hashLink url = do
    generator <- liftIO Random.newStdGen
    pure (Text.pack $ take 7 (Random.randomRs ('a', 'z') generator))