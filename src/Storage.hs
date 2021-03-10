{-# LANGUAGE FlexibleContexts #-}
module Storage where

import Models
import Typeclasses

import qualified Data.Text as T
import Control.Monad.Reader
import System.IO
import System.Directory
import Data.Binary
import Data.Text.Encoding.Base32 (encodeBase32)
import Control.Exception

-- Generic functions
isHidden :: String -> Bool
isHidden "." = True
isHidden ".." = True
isHidden r = False

addEntityFs pathFunc keyFunc entity = do
    dbPath <- asks appDbPath
    let entityPath = pathFunc dbPath $ (T.unpack . keyFunc) entity
    exists <- liftIO $ doesFileExist entityPath
    if exists
        then liftIO $ throw AlreadyExists
        else liftIO $ encodeFile entityPath entity

removeEntityFs pathFunc key = do
    dbPath <- asks appDbPath
    let entityPath = pathFunc dbPath $ T.unpack key
    exists <- liftIO $ doesFileExist entityPath
    if exists
        then liftIO $ removeFile entityPath
        else liftIO $ throw DoesNotExist

lookupEntityFs pathFunc decoder key = do
    dbPath <- asks appDbPath
    let entityPath = pathFunc dbPath $ T.unpack key
    exists <- liftIO $ doesFileExist entityPath
    if exists
        then do
            res <- liftIO $ decoder entityPath
            liftIO $ return $ Just res
        else pure Nothing

getAllEntitiesFs dir = do
    dbPath <- asks appDbPath
    files <- liftIO $ getDirectoryContents (dbPath ++ dir)
    liftIO $ mapM (\name -> decodeFile (dbPath ++ dir ++ "/" ++ name)) (filter (not . isHidden) files)

-- Users
userPath :: String -> String -> String
userPath base email = base ++ "/user/" ++ ((T.unpack . encodeBase32 . T.pack) email) ++ ".dat"

getAllUsersFs :: (MonadReader AppConfig m, MonadIO m) => m [User]
getAllUsersFs = getAllEntitiesFs "/user"
addUserFs :: (MonadReader AppConfig m, MonadIO m) => User -> m ()
addUserFs = addEntityFs userPath userEmail
lookupUserFs :: (MonadReader AppConfig m, MonadIO m) => Key User -> m (Maybe User)
lookupUserFs = lookupEntityFs userPath (decodeFile :: FilePath -> IO User)
removeUserFs :: (MonadReader AppConfig m, MonadIO m) => Key User -> m ()
removeUserFs = removeEntityFs userPath

-- Aliases
linkPath :: String -> String -> String
linkPath base alias = base ++ "/link/" ++ alias ++ ".dat"

getAllAliasesFs :: (MonadReader AppConfig m, MonadIO m) => m [Alias]
getAllAliasesFs = getAllEntitiesFs "/link"
addAliasFs :: (MonadReader AppConfig m, MonadIO m) => Alias -> m ()
addAliasFs = addEntityFs linkPath aliasName
lookupAliasFs :: (MonadReader AppConfig m, MonadIO m) => Key Alias -> m (Maybe Alias)
lookupAliasFs = lookupEntityFs linkPath (decodeFile :: FilePath -> IO Alias)
removeAliasFs :: (MonadReader AppConfig m, MonadIO m) => Key Alias -> m ()
removeAliasFs = removeEntityFs linkPath
