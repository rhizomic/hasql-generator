import Data.ByteString (readFile)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Traversable (mapM)
import Hasql.Generator.Internal.Database (migrate, withDb)
import Hasql.Generator.Internal.Database.Sql.ParserSpec qualified as ParserSpec
import Hasql.Generator.Internal.Database.Transaction (runTransaction)
import Hasql.Generator.Internal.DatabaseSpec qualified as DatabaseSpec
import Hasql.Generator.Internal.RendererSpec qualified as RendererSpec
import Hasql.GeneratorSpec qualified as GeneratorSpec
import Hasql.Pool (Pool, use)
import PgQuery hiding (String, action, view)
import System.IO (IO)
import Test.Hspec (describe, hspec)
import Test.Hspec.Expectations.Pretty (shouldNotBe)
import TestImport.Assertions (assertRight)
import TestImport.Migrations (getMigrationFiles)

main :: IO ()
main = do
  withMigratedDb $ \pool ->
    hspec $ do
      describe "Parser" ParserSpec.spec
      describe "Database" $ DatabaseSpec.spec pool
      describe "Renderer" RendererSpec.spec
      describe "Generator" GeneratorSpec.spec

withMigratedDb ::
  (Pool -> IO ()) ->
  IO ()
withMigratedDb action =
  withDb $ \pool -> do
    migrationFiles <- getMigrationFiles
    migrations <- mapM readFile migrationFiles
    migrations `shouldNotBe` []

    fmap assertRight <$> use pool . runTransaction $ do
      migrate migrations

    action pool
