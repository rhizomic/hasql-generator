import Control.Applicative (pure)
import Data.ByteString (readFile)
import Data.Either (Either (Left, Right))
import Data.Foldable (traverse_)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Monoid ((<>))
import Data.Text (unpack)
import Data.Traversable (mapM)
import GHC.Err (error)
import Hasql.Generator.Internal.Database (withDb)
import Hasql.Generator.Internal.Database.Sql.Parser2Spec qualified as Parser2Spec
import Hasql.Generator.Internal.Database.Sql.ParserSpec qualified as ParserSpec
import Hasql.Generator.Internal.Database.Transaction (paramAndResultlessTransaction, runTransaction)
import Hasql.Generator.Internal.DatabaseSpec qualified as DatabaseSpec
import Hasql.Generator.Internal.Renderer2Spec qualified as Renderer2Spec
import Hasql.Generator.Internal.RendererSpec qualified as RendererSpec
import Hasql.GeneratorSpec qualified as GeneratorSpec
import Hasql.Pool (Pool, use)
import System.IO (IO)
import Test.Hspec (describe, hspec)
import Test.Hspec.Expectations.Pretty (shouldNotBe)
import TestImport.Assertions (assertRight)
import TestImport.Migrations (getMigrationFiles)

main :: IO ()
main = do
  withMigratedDb $ \pool ->
    hspec $ do
      -- describe "Parser" ParserSpec.spec
      describe "Parser2" Parser2Spec.spec
      describe "Database" $ DatabaseSpec.spec pool
      -- describe "Renderer" RendererSpec.spec
      describe "Renderer2" Renderer2Spec.spec
      describe "Generator" GeneratorSpec.spec

withMigratedDb ::
  (Pool -> IO ()) ->
  IO ()
withMigratedDb action = do
  eResult <- withDb $ \_dbSettings pool -> do
    migrationFiles <- getMigrationFiles
    migrations <- mapM readFile migrationFiles
    migrations `shouldNotBe` []

    fmap assertRight <$> use pool . runTransaction $ do
      traverse_ paramAndResultlessTransaction migrations

    action pool

  case eResult of
    Left err -> error $ "Running tests failed with reason: " <> unpack err
    Right res -> pure res
