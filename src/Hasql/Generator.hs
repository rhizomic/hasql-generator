module Hasql.Generator (generate) where

import Control.Applicative (pure)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (Bool (False, True))
import Data.ByteString (readFile)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString, empty)
import Data.Either (Either (Left, Right), either)
import Data.Eq ((==))
import Data.Function (const, id, ($), (.))
import Data.Functor (fmap)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Maybe (Maybe (Just, Nothing), mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.IO (writeFile)
import Data.Traversable (traverse)
import GHC.IO (IO)
import GHC.Show (Show (show))
import Hasql.Generator.Internal.Database (withDb)
import Hasql.Generator.Internal.Database.Sql (parameterAndResultMetadata)
import Hasql.Generator.Internal.Database.Sql.Analysis2 (getParameterAndResultMetadata)
import Hasql.Generator.Internal.Database.Sql.Parser (parseLimit)
import Hasql.Generator.Internal.Database.Sql.Parser2 (parseQueryParameters, parseQueryResults, parseTableRelations)
import Hasql.Generator.Internal.Database.Transaction (runTransaction)
import Hasql.Generator.Internal.Database.Types (DatabaseSettings (host))
import Hasql.Generator.Internal.Renderer2 (toHaskell)
import Hasql.Generator.Types
  ( QueryConfig
      ( functionName,
        inputFile,
        moduleName,
        outputLocation
      ),
  )
import Hasql.Pool (Pool, use)
import PgQuery (parseSql)
import System.IO (FilePath)
import System.Process.Typed
  ( ExitCode,
    proc,
    readProcess,
  )

-- | Generates Hasql code for each of the provided 'QueryConfig's.
generate ::
  -- | The path to the schema file that should be executed before attempting to
  --   generate code.
  FilePath ->
  -- | The 'QueryConfig's to generate code for.
  NonEmpty QueryConfig ->
  IO (Either (NonEmpty (Text, QueryConfig)) ())
generate schemaFile queries = do
  eResult <- withDb $ \dbSettings pool -> do
    (_exitCode, _stdOut, stdErr) <- loadSchema dbSettings

    -- We ignore the exitCode because psql still reports a success even when
    -- encountering issues with executing the file. Instead, we use the
    -- absence of content in stdErr as a measure of success.
    case stdErr == empty of
      False ->
        let errorMessage =
              "Could not render Hasql code due to an issue loading the schema: " <> pack (show stdErr)
         in pure . Left $ queriesWithError errorMessage
      True -> do
        renderResults <- traverse (renderToFile pool) queries
        let renderingIssues = mapMaybe leftToMaybe (toList renderResults)
        case nonEmpty renderingIssues of
          Just issues -> pure $ Left issues
          Nothing -> pure $ Right ()

  pure $ either (Left . queriesWithError) id eResult
  where
    loadSchema :: (MonadIO m) => DatabaseSettings -> m (ExitCode, ByteString, ByteString)
    loadSchema dbSettings = do
      let cmd =
            proc
              "psql"
              [ "-h"
              , unpack dbSettings.host
              , "-f"
              , schemaFile
              ]

      readProcess cmd

    renderToFile :: Pool -> QueryConfig -> IO (Either (Text, QueryConfig) ())
    renderToFile pool query = do
      sql <- readFile query.inputFile
      eResult <- parameterAndResultMetadata pool sql

      case eResult of
        Left err ->
          let errorMessage =
                "Could not render Hasql code for "
                  <> pack (show query.inputFile)
                  <> ". Reason(s): "
                  <> err
           in pure $ Left (errorMessage, query)
        Right metadata -> do
          let result = toHaskell sql metadata query.moduleName query.functionName
          writeFile query.outputLocation result
          pure $ Right ()

    queriesWithError :: Text -> NonEmpty (Text, QueryConfig)
    queriesWithError errorMessage =
      fmap (errorMessage,) queries

    leftToMaybe :: Either a b -> Maybe a
    leftToMaybe = either Just (const Nothing)
