module Hasql.Generator.Internal.Database.Statement
  ( preparedStatement,
    unpreparedStatement,
    paramlessStatement,
    resultlessStatement,
    paramAndResultlessStatement,
  )
where

import Data.Bool (Bool (False, True))
import Data.ByteString.Char8 (ByteString)
import Hasql.Decoders (Result, noResult)
import Hasql.Encoders (Params, noParams)
import Hasql.Statement (Statement (Statement))

-- | Creates a prepared 'Statement'.
preparedStatement ::
  ByteString ->
  Params params ->
  Result result ->
  Statement params result
preparedStatement sql paramEncoder resultDecoder =
  Statement sql paramEncoder resultDecoder True

-- | Creates an unprepared 'Statement'.
unpreparedStatement ::
  ByteString ->
  Params params ->
  Result result ->
  Statement params result
unpreparedStatement sql paramEncoder resultDecoder =
  Statement sql paramEncoder resultDecoder False

-- | Creates a prepared 'Statement' which lacks parameters but produces
--   results.
paramlessStatement ::
  ByteString ->
  Result result ->
  Statement () result
paramlessStatement sql =
  preparedStatement sql noParams

-- | Creates a prepared 'Statement' which provides parameters but produces
--   no results.
resultlessStatement ::
  ByteString ->
  Params params ->
  Statement params ()
resultlessStatement sql paramEncoder =
  preparedStatement sql paramEncoder noResult

-- | Creates a prepared 'Statement' which lacks parameters and produces
--   no results.
paramAndResultlessStatement ::
  ByteString ->
  Statement () ()
paramAndResultlessStatement sql =
  preparedStatement sql noParams noResult
