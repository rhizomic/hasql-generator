module Hasql.Generator.Internal.Database.Sql
  ( parameterAndResultMetadata,
  )
where

import Data.ByteString (ByteString)
import Data.Function (($))
import Data.Functor (fmap)
import Data.Text.Encoding (decodeUtf8)
import Hasql.Generator.Internal.Database.Sql.Analysis
  ( getParameterAndResultMetadata,
    getParameterAndResultTypes,
    getPreparedStatementDetails,
  )
import Hasql.Generator.Internal.Database.Sql.Parser (parseJoins)
import Hasql.Generator.Internal.Database.Sql.PreparedStatement
  ( withPreparedSql,
  )
import Hasql.Generator.Internal.Database.Sql.Types
  ( PostgresqlParameterAndResultMetadata,
    PostgresqlParameterAndResultTypeReplacements (parameterTypes),
    PostgresqlTypeReplacement (replacement),
  )
import Hasql.Transaction (Transaction)

-- | Retrieves the 'PostgresqlParameterAndResultMetadata' for a given query.
parameterAndResultMetadata ::
  ByteString ->
  Transaction PostgresqlParameterAndResultMetadata
parameterAndResultMetadata sql = do
  withPreparedSql sql $ \preparedStatementName -> do
    parameterAndResultTypes <- getParameterAndResultTypes preparedStatementName
    let parameterTypes =
          fmap (.replacement) parameterAndResultTypes.parameterTypes

    preparedStatementDetails <- getPreparedStatementDetails preparedStatementName parameterTypes

    let joins = parseJoins $ decodeUtf8 sql

    getParameterAndResultMetadata
      parameterAndResultTypes
      joins
      preparedStatementDetails
