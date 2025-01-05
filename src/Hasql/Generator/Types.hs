module Hasql.Generator.Types
  ( QueryConfig (..),
    EnumConfig (..),
    HasqlEnum (..),
  )
where

import Data.Function ((.))
import Data.Maybe (Maybe)
import Data.Text (Text, pack, unpack)
import GHC.Read (Read)
import GHC.Show (Show, show)
import System.IO (FilePath)
import Text.Read (readMaybe)

-- | Outlines the configuration necessary to generate Hasql code for a SQL
--   file.
data QueryConfig = QueryConfig
  { inputFile :: FilePath
  -- ^ The location of the SQL that should have code generated for it.
  , outputLocation :: FilePath
  -- ^ The location where the generated code should be emitted.
  , moduleName :: Text
  -- ^ The module name to use in the generated code.
  , functionName :: Text
  -- ^ The function name to use in the generated code.
  }
  deriving stock (Show)

-- | The configuration for a PostgreSQL enum.
data EnumConfig = EnumConfig
  { moduleName :: Text
  -- ^ The name of the module where the 'haskellType' is defined, along with
  --   its 'HasqlEnum' instance.
  , haskellType :: Text
  -- ^ The name of the type that encodes the enum on the Haskell side.
  }
  deriving stock (Show)

-- | 'HasqlEnum' is a typeclass used to serialize Haskell sum types into
--   Postgres and deserialize Postgres enums back into Haskell.
--
--   Default implementations are provided for 'hsToPg' ('show') and
--   'pgToHs' ('readMaybe').
class HasqlEnum t where
  -- | Converts the Haskell enum into a PostgreSQL enum.
  hsToPg :: t -> Text
  default hsToPg :: (Show t) => t -> Text
  hsToPg = pack . show

  -- | Converts the PostgreSQL enum into a Haskell enum.
  pgToHs :: Text -> Maybe t
  default pgToHs :: (Read t) => Text -> Maybe t
  pgToHs = readMaybe . unpack
