module Hasql.Generator.Internal.Database.Transaction
  ( paramlessTransaction,
    resultlessTransaction,
    paramAndResultlessTransaction,
    transaction,
    runTransaction,
  )
where

import Data.ByteString.Char8 (ByteString)
import Hasql.Decoders (Result)
import Hasql.Encoders (Params)
import Hasql.Generator.Internal.Database.Statement
  ( paramAndResultlessStatement,
    paramlessStatement,
    preparedStatement,
    resultlessStatement,
  )
import Hasql.Session (Session)
import Hasql.Transaction
  ( Transaction,
    statement,
  )
import Hasql.Transaction.Sessions
  ( IsolationLevel (ReadCommitted),
    Mode (Write),
  )
import Hasql.Transaction.Sessions qualified as Session
  ( transaction,
  )

-- | Creates a prepared 'Transaction' which provides parameters and produces
--   results.
transaction ::
  ByteString ->
  a ->
  Params a ->
  Result result ->
  Transaction result
transaction sql params paramEncoder resultDecoder =
  statement params (preparedStatement sql paramEncoder resultDecoder)

-- | Creates a prepared 'Transaction' which lacks parameters but produces
--   results.
paramlessTransaction ::
  ByteString ->
  Result result ->
  Transaction result
paramlessTransaction sql resultDecoder =
  statement () (paramlessStatement sql resultDecoder)

-- | Creates a prepared 'Transaction' which provides parameters but produces
--   no results.
resultlessTransaction ::
  ByteString ->
  a ->
  Params a ->
  Transaction ()
resultlessTransaction sql params paramEncoder =
  statement params (resultlessStatement sql paramEncoder)

-- | Creates a prepared 'Transaction' which lacks parameters and produces
--   no results.
paramAndResultlessTransaction ::
  ByteString ->
  Transaction ()
paramAndResultlessTransaction sql =
  statement () (paramAndResultlessStatement sql)

-- | Runs a transaction with read/write access and an isolation level of
--   `Read Committed`.
--
--   * https://www.postgresql.org/docs/current/transaction-iso.html
--   * https://www.postgresql.org/docs/current/sql-set-transaction.html
runTransaction :: Transaction a -> Session a
runTransaction = Session.transaction ReadCommitted Write
