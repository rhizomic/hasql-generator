module Hasql.Generator (generate) where

import Control.Applicative (pure)
import Data.ByteString (readFile)
import Data.ByteString.Char8 (unpack)
import Data.Either (Either (Left, Right), either)
import Data.Function (const, id, ($), (.))
import Data.Functor (fmap)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Maybe (Maybe (Just, Nothing), mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, intercalate, pack)
import Data.Text.IO (writeFile)
import Data.Traversable (mapM, traverse)
import GHC.IO (IO)
import GHC.Show (Show (show))
import Hasql.Generator.Internal.Database (migrate, withDb)
import Hasql.Generator.Internal.Database.Sql (parameterAndResultMetadata)
import Hasql.Generator.Internal.Database.Sql.Parser (parseLimit)
import Hasql.Generator.Internal.Database.Transaction (runTransaction)
import Hasql.Generator.Internal.Renderer (renderingIssueToHuman, toHaskell)
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

-- | Generates Hasql code for each of the provided 'QueryConfig's.
generate ::
  -- | The paths to the migrations that should be run before attempting to
  --   generate code.
  [FilePath] ->
  -- | The 'QueryConfig's to generate code for.
  NonEmpty QueryConfig ->
  IO (Either (NonEmpty (Text, QueryConfig)) ())
generate migrationFiles queries = do
  migrations <- mapM readFile migrationFiles

  eResult <- withDb $ \pool -> do
    eMigrationResult <- use pool . runTransaction $ do
      migrate migrations

    case eMigrationResult of
      Left err ->
        let errorMessage =
              "Could not render Hasql code due to a migration failure. Reason: " <> pack (show err)
         in pure . Left $ queriesWithError errorMessage
      Right () -> do
        renderResults <- traverse (renderToFile pool) queries
        let renderingIssues = mapMaybe leftToMaybe (toList renderResults)
        case nonEmpty renderingIssues of
          Just issues -> pure $ Left issues
          Nothing -> pure $ Right ()

  pure $ either (Left . queriesWithError) id eResult
  where
    renderToFile :: Pool -> QueryConfig -> IO (Either (Text, QueryConfig) ())
    renderToFile pool query = do
      sql <- readFile query.inputFile
      eParseResult <- parseSql $ unpack sql
      -- TODO: Clean all this branching up
      case eParseResult of
        Left err ->
          pure $ Left (toError (pack $ show err), query)
        Right parseResult -> do
          let mLimit = parseLimit parseResult

          eMetadata <-
            use pool . runTransaction $
              parameterAndResultMetadata sql

          case eMetadata of
            Left err ->
              pure $ Left (toError (pack $ show err), query)
            Right metadata ->
              case toHaskell sql metadata mLimit query.moduleName query.functionName of
                Left renderingIssues ->
                  let errors = intercalate "\n" $ fmap renderingIssueToHuman renderingIssues
                   in pure $ Left (toError errors, query)
                Right result -> do
                  writeFile query.outputLocation result
                  pure $ Right ()
      where
        toError :: Text -> Text
        toError err =
          "Could not render Hasql code for "
            <> pack (show query.inputFile)
            <> ". Reason(s): "
            <> err

    queriesWithError :: Text -> NonEmpty (Text, QueryConfig)
    queriesWithError errorMessage =
      fmap (errorMessage,) queries

    leftToMaybe :: Either a b -> Maybe a
    leftToMaybe = either Just (const Nothing)
