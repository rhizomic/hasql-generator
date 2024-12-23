module Hasql.Generator.Internal.Database.Types
  ( DatabaseSettings (..),
  )
where

import Data.ByteString (ByteString)
import GHC.Show (Show)

-- | A collection of database settings. All that's currently required for this
--   to work with `tmp-postgres` is a host name.
newtype DatabaseSettings = DatabaseSettings
  { host :: ByteString
  -- ^ The database host.
  }
  deriving stock (Show)
