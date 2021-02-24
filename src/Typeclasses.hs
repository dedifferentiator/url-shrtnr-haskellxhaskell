module Typeclasses where

import Control.Monad.IO.Class (MonadIO)
import Data.Text
import Models

type UserKey = Email

type LinkKey = Text

class (MonadIO m) => Database m where
  getAllUsers :: m [User]
  getAllLinks :: m [Link]

  addUser :: User -> m Bool
  removeUser :: UserKey -> m Bool

  addLink :: Link -> m Bool
  removeLink :: LinkKey -> m Bool
