{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Urls (annihilateAlias, createAlias, getUrls) where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString as ByteString
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Models
import Typeclasses

annihilateAlias ::
  (Database m) =>
  Key Alias ->
  m (Either AppError ())
annihilateAlias alias = do
  mExisted <- lookupAlias alias

  if isJust mExisted
    then do
      removeAlias alias
      pure (Right ())
    else pure (Left DeletingError)

createAlias ::
  (Database m, Hasher m) =>
  AliasOrigin ->
  Maybe AliasName ->
  Email ->
  m (Either AppError AliasName)
createAlias url alias email = do
  let getaName =
        ( \case
            Just aName -> pure aName
            Nothing -> hashLink url
        )
  aName <- getaName alias
  mAlias <- lookupAlias aName
  if isNothing mAlias
    then do
      addAlias (Alias url aName email)
      pure (Right aName)
    else pure (Left CreatingError)

getUrls ::
  (Database m) =>
  Email ->
  m [AliasName]
getUrls email = do
  aList <- getAllAliases
  pure $ map aliasName (filter (\x -> aliasAuthor x == email) aList)
