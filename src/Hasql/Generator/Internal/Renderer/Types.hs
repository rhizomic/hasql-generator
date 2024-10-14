module Hasql.Generator.Internal.Renderer.Types (RenderingIssue (..)) where

import Data.Eq (Eq)
import Data.Text (Text)
import GHC.Show (Show)

-- | This type captures issues that come up when trying to render the Hasql
--   code for a given query.
data RenderingIssue
  = -- | One of the 'FragmentMetadata's contains an 'UnknownFragment'.
    FragmentMetadataContainsUnknownFragment Text
  | -- | One of the 'FragmentMetadata's contains a 'PgUnknown'.
    FragmentMetadataContainsUnknownType Text
  deriving stock (Show, Eq)
