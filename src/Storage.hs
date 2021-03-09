{-# LANGUAGE FlexibleContexts #-}
module Storage ()
where

import Models
import Typeclasses

import qualified Data.Text as T
import Control.Monad.Reader
import System.IO
import System.Directory
import Data.Binary
import Data.Text.Encoding.Base32 (encodeBase32)

-- Generic functions
addEntityFs pathFunc keyFunc entity = do
    dbPath <- asks appDbPath
    liftIO $ encodeFile (pathFunc dbPath $ (T.unpack . keyFunc) entity) entity

removeEntityFs pathFunc key = do
    dbPath <- asks appDbPath
    liftIO $ removeFile (pathFunc dbPath (T.unpack key))

lookupEntityFs pathFunc key = do
    dbPath <- asks appDbPath
    liftIO $ decodeFile (pathFunc dbPath (T.unpack key))

getAllEntitiesFs dir = do
    dbPath <- asks appDbPath
    files <- liftIO $ getDirectoryContents (dbPath ++ dir)
    liftIO $ mapM decodeFile files

-- Users
userPath :: String -> String -> String
userPath base email = base ++ "/user/" ++ ((T.unpack . encodeBase32 . T.pack) email) ++ ".dat"

getAllUsersFs :: (MonadReader AppConfig m, MonadIO m) => m [User]
getAllUsersFs = getAllEntitiesFs "/user"
addUserFs :: (MonadReader AppConfig m, MonadIO m) => User -> m ()
addUserFs = addEntityFs userPath userEmail
lookupUserFs :: (MonadReader AppConfig m, MonadIO m) => Key User -> m User
lookupUserFs = lookupEntityFs userPath
removeUserFs :: (MonadReader AppConfig m, MonadIO m) => Key User -> m ()
removeUserFs = removeEntityFs userPath

-- Aliases
linkPath :: String -> String -> String
linkPath base alias = base ++ "/link/" ++ alias ++ ".dat"

getAllAliasesFs :: (MonadReader AppConfig m, MonadIO m) => m [Alias]
getAllAliasesFs = getAllEntitiesFs "/link"
addAliasFs :: (MonadReader AppConfig m, MonadIO m) => Alias -> m ()
addAliasFs = addEntityFs linkPath aliasAlias
lookupAliasFs :: (MonadReader AppConfig m, MonadIO m) => Key Alias -> m Alias
lookupAliasFs = lookupEntityFs linkPath
removeAliasFs :: (MonadReader AppConfig m, MonadIO m) => Key Alias -> m ()
removeAliasFs = removeEntityFs linkPath
