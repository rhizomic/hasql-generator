module Hasql.Generator.Types (QueryConfig (..)) where

import Data.Text (Text)
import GHC.Show (Show)
import System.IO (FilePath)

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
