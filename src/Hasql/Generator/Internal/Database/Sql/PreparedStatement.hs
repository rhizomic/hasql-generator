module Hasql.Generator.Internal.Database.Sql.PreparedStatement
  ( withPreparedSql,
  )
where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Hasql.Generator.Internal.Database.Transaction (paramAndResultlessTransaction)
import Hasql.Transaction (Transaction)

-- | Prepares the provided sql and then runs an action with the name of the
--   prepared statement.
withPreparedSql ::
  ByteString ->
  (ByteString -> Transaction a) ->
  Transaction a
withPreparedSql sql action = do
  prepare preparedStatementName sql
  results <- action preparedStatementName
  deallocate preparedStatementName
  pure results
  where
    preparedStatementName :: ByteString
    preparedStatementName = "___tmp_prepared_statement___"

-- | Prepares the provided SQL, using the provided name.
prepare ::
  ByteString ->
  ByteString ->
  Transaction ()
prepare statementName sql =
  paramAndResultlessTransaction ("prepare " <> statementName <> " as " <> sql)

-- | Deallocates a prepared statement.
deallocate ::
  ByteString ->
  Transaction ()
deallocate preparedStatementName =
  paramAndResultlessTransaction ("deallocate " <> preparedStatementName)
