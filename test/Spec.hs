{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import Authentication
import Control.Exception (evaluate)
import Control.Monad.Reader
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.List
import qualified Data.Text as Text
import Models
import Test.Hspec
-- import Test.Hspec.Wai
-- import Test.Hspec.Wai.JSON
import Typeclasses
import Urls

main :: IO ()
main = hspec $ do
  registerUserSpec
  annihilateAliasSpec
  getUrlsSpec
  createAliasSpec

type TestState = ([User], [Alias])

type TestM = ReaderT AppConfig (State TestState)

instance Hasher TestM where
  hashPassword = pure . Just
  validatePassword hash pass = pure (hash == pass)
  hashLink = pure

instance Database TestM where
  getAllUsers = State.gets fst
  getAllAliases = State.gets snd

  addUser user = do
    (users, aliases) <- State.get
    State.put (user : users, aliases)
  lookupUser email = do
    (users, _) <- State.get
    pure $ find (\x -> userEmail x == email) users
  removeUser email = do
    (users, aliases) <- State.get
    State.put (filter (\x -> userEmail x /= email) users, aliases)

  addAlias alias = do
    (users, aliases) <- State.get
    State.put (users, alias : aliases)
  lookupAlias alias = do
    (_, aliases) <- State.get
    pure $ find (\x -> aliasName x == alias) aliases
  removeAlias alias = do
    (users, aliases) <- State.get
    State.put (users, filter (\x -> aliasName x /= alias) aliases)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig 3001

runTest :: AppConfig -> TestState -> TestM r -> (r, TestState)
runTest config state m = State.runState (runReaderT m config) state

runEmptyTest :: TestM r -> (r, TestState)
runEmptyTest = runTest defaultAppConfig ([], [])

registerUserSpec =
  describe "Authentication.registerUser" $ do
    let email = Text.pack "email"
        pass = Text.pack "pass"
    it "successfully creates a new user" $ do
      snd (runEmptyTest (registerUser email pass))
        `shouldBe` ([User email pass], [])

    it "returns an error when the user is already registered" $ do
      fst (runTest defaultAppConfig ([User email pass], []) (registerUser email pass))
        `shouldBe` Left RegistrationError

annihilateAliasSpec =
  describe "Urls.annihilateAlias" $ do
    let name = Text.pack "name"
        email = Text.pack "email"
        origin = Text.pack "origin"
    it "successfully deletes alias" $ do
      snd (runTest defaultAppConfig ([], [Alias origin name email]) (annihilateAlias name))
        `shouldBe` ([], [])

    it "returns an error when try to delete unexisted alias" $ do
      fst (runEmptyTest (annihilateAlias name))
        `shouldBe` Left DeletingError

getUrlsSpec =
  describe "Urls.getUrls" $ do
    let email1 = Text.pack "email1"
        email2 = Text.pack "email2"
        origin = Text.pack "origin"
        name1 = email1
        name2 = email2

    it "succesfully gets urls from the correct user" $ do
      fst (runTest defaultAppConfig ([], [Alias origin name1 email1, Alias origin name2 email2]) (getUrls email1))
        `shouldBe` [name1]

createAliasSpec =
  describe "Urls.createAlias" $ do
    let email = Text.pack "email"
        name = Text.pack "name"
        origin = Text.pack "origin"
    it "successfully creates a new alias" $ do
      snd (runEmptyTest (createAlias origin (Just name) email))
        `shouldBe` ([], [Alias origin name email])

    it "returns an error when the alias already exists" $ do
      fst (runTest defaultAppConfig ([], [Alias origin name email]) (createAlias origin (Just name) email))
        `shouldBe` Left CreatingError

    it "creates and returns new alias when no AliasName is given" $ do
      runEmptyTest (createAlias origin Nothing email)
        `shouldBe` (Right origin, ([], [Alias origin origin email]))
