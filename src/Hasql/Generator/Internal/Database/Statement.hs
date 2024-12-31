module Hasql.Generator.Internal.Database.Statement
  ( preparedStatement,
    paramAndResultlessStatement,
  )
where

import Data.Bool (Bool (True))
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

-- | Creates a prepared 'Statement' which lacks parameters and produces
--   no results.
paramAndResultlessStatement ::
  ByteString ->
  Statement () ()
paramAndResultlessStatement sql =
  preparedStatement sql noParams noResult
