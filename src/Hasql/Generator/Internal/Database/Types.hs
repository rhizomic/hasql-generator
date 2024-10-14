module Hasql.Generator.Internal.Database.Types
  ( DatabaseSettings (..),
  )
where

import Data.ByteString (ByteString)
import GHC.Show (Show)

-- | A collection of database settings.
data DatabaseSettings = DatabaseSettings
  { host :: ByteString
  -- ^ The database host.
  , port :: ByteString
  -- ^ The database port.
  , user :: ByteString
  -- ^ The database user.
  , databaseName :: ByteString
  -- ^ The database name.
  }
  deriving stock (Show)
