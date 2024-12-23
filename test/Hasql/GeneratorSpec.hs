module Hasql.GeneratorSpec (spec) where

import Control.Applicative (pure)
import Data.Either (Either (Left, Right))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List.NonEmpty (singleton, toList)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (intercalate, unpack)
import Data.Tuple (fst)
import GHC.Base (error)
import GHC.IO (IO)
import Hasql.Generator (generate)
import Hasql.Generator.Types
  ( QueryConfig
      ( QueryConfig,
        functionName,
        inputFile,
        moduleName,
        outputLocation
      ),
  )
import System.Directory (removeFile)
import System.FilePath (takeFileName)
import System.IO (FilePath, readFile)
import System.IO.Temp (withTempDirectory)
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Golden (Golden, defaultGolden)
import TestImport.Migrations (getMigrationFiles, migrationFilesToSchemaFile)

spec :: Spec
spec = do
  describe "generate" do
    it "generates the code for select_country_from_addresses.sql" $ do
      testGeneratedCode "test/sql/select_country_from_addresses.sql"

    it "generates the code for select_distinct_country_from_addresses.sql" $ do
      testGeneratedCode "test/sql/select_distinct_country_from_addresses.sql"

    it "generates the code for user_info.sql" $ do
      testGeneratedCode "test/sql/user_info.sql"

testGeneratedCode :: FilePath -> IO (Golden String)
testGeneratedCode inputFile = do
  migrationFiles <- getMigrationFiles
  schemaFile <- migrationFilesToSchemaFile migrationFiles

  withTempDirectory "." "hasql-generator-spec" $ \tmpDir -> do
    let queryConfig = toQueryConfig tmpDir inputFile
    actual <- generate schemaFile (singleton queryConfig)
    removeFile schemaFile

    case actual of
      Left renderingIssues ->
        let issues = intercalate "\n" . toList $ fmap fst renderingIssues
         in error ("Unexpected issue rendering test code: " <> unpack issues)
      Right () -> do
        contents <- readFile queryConfig.outputLocation
        pure $ defaultGolden inputFile contents

toQueryConfig :: FilePath -> FilePath -> QueryConfig
toQueryConfig tmpDir inputFile =
  let outputLocation = tmpDir <> "/" <> takeFileName inputFile
   in QueryConfig
        { inputFile = inputFile
        , outputLocation = outputLocation
        , moduleName = "Query"
        , functionName = "query"
        }
