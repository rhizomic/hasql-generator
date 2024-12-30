module Hasql.Generator.Internal.Database.Sql
  ( parameterAndResultMetadata,
  )
where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Either (Either (Left, Right))
import Data.Either.Extra (mapLeft)
import Data.Function (($), (.))
import Data.Text (Text, pack)
import GHC.IO (IO)
import GHC.Show (show)
import Hasql.Generator.Internal.Database.Sql.Analysis2 (getParameterAndResultMetadata)
import Hasql.Generator.Internal.Database.Sql.Analysis2.Types (PostgresqlParameterAndResultMetadata)
import Hasql.Generator.Internal.Database.Sql.Parser2 (parseLimit, parseQueryParameters, parseQueryResults, parseTableRelations)
import Hasql.Generator.Internal.Database.Transaction (runTransaction)
import Hasql.Pool (Pool, use)
import PgQuery (parseSql)

-- | Retrieves the 'PostgresqlParameterAndResultMetadata' for a given query.
parameterAndResultMetadata ::
  Pool ->
  ByteString ->
  IO (Either Text PostgresqlParameterAndResultMetadata)
parameterAndResultMetadata pool sql = do
  eParseResult <- parseSql $ unpack sql
  case eParseResult of
    Left err ->
      pure . Left $ pack err
    Right parseResult -> do
      let mLimit = parseLimit parseResult
          mTableRelations = parseTableRelations parseResult
          mQueryParameters = parseQueryParameters parseResult
          mQueryResults = parseQueryResults parseResult

      eMetadata <-
        use pool . runTransaction $
          getParameterAndResultMetadata
            mLimit
            mTableRelations
            mQueryParameters
            mQueryResults

      pure $ mapLeft (pack . show) eMetadata
