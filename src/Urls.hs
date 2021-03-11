module Urls (redirectUser) where

import Models
import Typeclasses

redirectUser :: (Database m) => AliasName -> m (Maybe AliasOrigin)
redirectUser alias = (fmap aliasOrigin) <$> (lookupAlias alias)
