module TestImport.Migrations (getMigrationFiles) where

import Control.Applicative (pure)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (sort)
import Data.Monoid ((<>))
import System.Directory (listDirectory)
import System.IO (FilePath, IO)

getMigrationFiles :: IO [FilePath]
getMigrationFiles = do
  let migrationDir :: FilePath = "test/migrations/"
  files <- listDirectory migrationDir
  pure . sort $ fmap (migrationDir <>) files
