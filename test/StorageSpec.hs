module StorageSpec where

import System.Directory
import Control.Monad.ListM
import Control.Monad.Catch
import Data.Binary
import Data.Text.Encoding.Base32 (encodeBase32)
import Control.Monad.Reader
import qualified Data.Text as Text
import System.IO.Temp
import Data.Either
import Test.Hspec

import Typeclasses
import Models
import Storage

type TestFsM = ReaderT AppConfig IO

withTempAppConfig f = withTempDirectory "/tmp" "hspec." $ \dir -> do
    createDirectory (dir ++ "/link")
    createDirectory (dir ++ "/user")
    liftIO $ runReaderT f (AppConfig 3001 dir)

storageRemoveUserSpec =
    describe "Storage.removeUserFs" $ do
        let u1 = User (Text.pack "user1") (Text.pack "hash1")
            u2 = User (Text.pack "user2") (Text.pack "hash2")

        it "removes user files" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addUserFs u2
                    addUserFs u1
                    removeUserFs (Text.pack "user1")
                    files <- liftIO $ getDirectoryContents (dir ++ "/user")
                    filterM (\d -> pure $ not $ d `elem` [".", ".."]) files
                ) :: IO (Either SomeException [FilePath])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` ["OVZWK4RS.dat"]

        it "fails to remove non-existent users" $ do
            r <- try (withTempAppConfig (removeUserFs (Text.pack "user1"))) :: IO (Either SomeException ())
            case r of
                Left err -> fromException err `shouldBe` Just DoesNotExist
                Right res -> expectationFailure "Did not report error"

storageRemoveAliasSpec =
    describe "Storage.removeAliasFs" $ do
        let a1 = Alias (Text.pack "org1") (Text.pack "link1") (Text.pack "author1")
            a2 = Alias (Text.pack "org2") (Text.pack "link2") (Text.pack "author2")

        it "removes alias files" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addAliasFs a2
                    addAliasFs a1
                    removeAliasFs (Text.pack "link1")
                    files <- liftIO $ getDirectoryContents (dir ++ "/link")
                    filterM (\d -> pure $ not $ d `elem` [".", ".."]) files
                ) :: IO (Either SomeException [FilePath])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` ["link2.dat"]

        it "fails to remove non-existent aliases" $ do
            r <- try (withTempAppConfig (removeAliasFs (Text.pack "link1"))) :: IO (Either SomeException ())
            case r of
                Left err -> fromException err `shouldBe` Just DoesNotExist
                Right res -> expectationFailure "Did not report error"

storageLookupUserSpec =
    describe "Storage.lookupUserFs" $ do
        let u1 = User (Text.pack "user1") (Text.pack "hash1")
            u2 = User (Text.pack "user2") (Text.pack "hash2")

        it "reports existing users" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addUserFs u2
                    addUserFs u1
                    u1' <- lookupUserFs (Text.pack "user1")
                    u2' <- lookupUserFs (Text.pack "user2")
                    pure [u1', u2']
                ) :: IO (Either SomeException [Maybe User])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` [Just u1, Just u2]

        it "doesn't report non-existent users" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addUserFs u1
                    u1' <- lookupUserFs (Text.pack "user1")
                    u2' <- lookupUserFs (Text.pack "user2")
                    pure [u1', u2']
                ) :: IO (Either SomeException [Maybe User])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` [Just u1, Nothing]

storageLookupAliasSpec =
    describe "Storage.lookupAliasFs" $ do
        let a1 = Alias (Text.pack "org1") (Text.pack "link1") (Text.pack "author1")
            a2 = Alias (Text.pack "org2") (Text.pack "link2") (Text.pack "author2")

        it "reports existing aliases" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addAliasFs a2
                    addAliasFs a1
                    a1' <- lookupAliasFs (Text.pack "link1")
                    a2' <- lookupAliasFs (Text.pack "link2")
                    pure [a1', a2']
                ) :: IO (Either SomeException [Maybe Alias])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` [Just a1, Just a2]

        it "doesn't report non-existent aliases" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addAliasFs a1
                    a1' <- lookupAliasFs (Text.pack "link1")
                    a2' <- lookupAliasFs (Text.pack "link2")
                    pure [a1', a2']
                ) :: IO (Either SomeException [Maybe Alias])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` [Just a1, Nothing]

storageAddUserSpec =
    describe "Storage.addUserFs" $ do
        let u1 = User (Text.pack "user1") (Text.pack "hash1")
            u2 = User (Text.pack "user2") (Text.pack "hash2")

        it "creates user files" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addUserFs u2
                    addUserFs u1
                    files <- liftIO $ getDirectoryContents (dir ++ "/user")
                    userFiles <- filterM (\d -> pure $ not $ d `elem` [".", ".."]) files
                    u1' <- liftIO $ decodeFile (dir ++ "/user/OVZWK4RR.dat")
                    u2' <- liftIO $ decodeFile (dir ++ "/user/OVZWK4RS.dat")
                    pure (userFiles, [u1', u2'])
                ) :: IO (Either SomeException ([FilePath], [User]))
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` (["OVZWK4RR.dat", "OVZWK4RS.dat"], [u1, u2])

        it "fails to create duplicate files" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addUserFs u1
                    addUserFs u1
                ) :: IO (Either SomeException ())
            case r of
                Left err -> fromException err `shouldBe` Just AlreadyExists
                Right res -> expectationFailure "Did not report error"

storageAddAliasSpec =
    describe "Storage.addAliasFs" $ do
        let a1 = Alias (Text.pack "org1") (Text.pack "link1") (Text.pack "author1")
            a2 = Alias (Text.pack "org2") (Text.pack "link2") (Text.pack "author2")

        it "creates alias files" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addAliasFs a2
                    addAliasFs a1
                    files <- liftIO $ getDirectoryContents (dir ++ "/link")
                    aliasFiles <- filterM (\d -> pure $ not $ d `elem` [".", ".."]) files
                    a1' <- liftIO $ decodeFile (dir ++ "/link/link1.dat")
                    a2' <- liftIO $ decodeFile (dir ++ "/link/link2.dat")
                    pure (aliasFiles, [a1', a2'])
                ) :: IO (Either SomeException ([FilePath], [Alias]))
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` (["link2.dat", "link1.dat"], [a1, a2])

        it "fails to create duplicate files" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addAliasFs a1
                    addAliasFs a1
                ) :: IO (Either SomeException ())
            case r of
                Left err -> fromException err `shouldBe` Just AlreadyExists
                Right res -> expectationFailure "Did not report error"

storageGetAllUsersSpec =
    describe "Storage.getAllUsersFs" $ do
        let u1 = User (Text.pack "user1") (Text.pack "hash1")
            u2 = User (Text.pack "user2") (Text.pack "hash2")

        it "properly handles empty storage" $ do
            r <- try (withTempAppConfig getAllUsersFs) :: IO (Either SomeException [User])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` []

        it "returns users" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addUserFs u2
                    addUserFs u1
                    getAllUsersFs
                ) :: IO (Either SomeException [User])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` [u1, u2]

storageGetAllAliasesSpec =
    describe "Storage.getAllAliasesFs" $ do
        let a1 = Alias (Text.pack "org1") (Text.pack "link1") (Text.pack "author1")
            a2 = Alias (Text.pack "org2") (Text.pack "link2") (Text.pack "author2")

        it "properly handles empty storage" $ do
            r <- try (withTempAppConfig getAllAliasesFs) :: IO (Either SomeException [Alias])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` []

        it "returns aliases" $ do
            r <- try (withTempAppConfig $ do
                    dir <- asks appDbPath
                    addAliasFs a2
                    addAliasFs a1
                    getAllAliasesFs
                ) :: IO (Either SomeException [Alias])
            case r of
                Left err -> expectationFailure ("Failure: " ++ (show err))
                Right res -> res `shouldBe` [a2, a1]

