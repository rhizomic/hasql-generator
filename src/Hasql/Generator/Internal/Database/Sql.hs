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
import Hasql.Generator.Internal.Database.Sql.Analysis (getParameterAndResultMetadata)
import Hasql.Generator.Internal.Database.Sql.Analysis.Types (PostgresqlParameterAndResultMetadata)
import Hasql.Generator.Internal.Database.Sql.Parser (parseNumberOfRowsReturned, parseQueryParameters, parseQueryResults, parseTableRelations)
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
      let mTableRelations = parseTableRelations parseResult
          mQueryParameters = parseQueryParameters parseResult
          mQueryResults = parseQueryResults parseResult
          numberOfRowsReturned = parseNumberOfRowsReturned parseResult

      eMetadata <-
        use pool . runTransaction $
          getParameterAndResultMetadata
            mTableRelations
            mQueryParameters
            mQueryResults
            numberOfRowsReturned

      pure $ mapLeft (pack . show) eMetadata
