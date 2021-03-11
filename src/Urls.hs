module Urls (redirectUser) where

import Models
import Typeclasses

redirectUser :: (Database m) => AliasName -> m (Maybe AliasOrigin)
redirectUser alias = fmap (fmap aliasOrigin) (lookupAlias alias)
