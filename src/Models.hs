module Models where

import Data.ByteString.Char8 (ByteString)
import Data.Text
import Data.Time

data User = User
  { userEmail :: Email,
    userHash :: ByteString,
    userTokens :: [Token]
  }
  deriving (Show, Eq)

data Token = Token JWT UTCTime
  deriving (Show, Eq)

data Alias = Alias
  { aliasOrigin :: Url,
    aliasAlias :: Text,
    aliasAuthor :: Email
  }

-- REVIEW: Is there a servant type for this?
type Url = String

type Password = String

-- TODO: There definitely is a servant type for this
type JWT = String

type Email = Text
