{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typeclasses where

import Models

newtype Key a = Key a
  deriving (Eq, Show)

class (Monad m) => Database m where
  getAllUsers :: m [User]
  getAllAliases :: m [Alias]

  addUser :: User -> m ()
  lookupUser :: Key User -> m User
  removeUser :: Key User -> m ()

  addAlias :: Alias -> m ()
  lookupAlias :: Key Alias -> m Alias
  removeAlias :: Key Alias -> m ()

class (Monad m) => Logger m where
  logInfo :: String -> m ()
  logWarning :: String -> m ()
  logError :: String -> m ()
