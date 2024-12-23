module TestImport.Migrations
  ( getMigrationFiles,
    migrationFilesToSchemaFile,
  )
where

import Control.Applicative (pure)
import Control.Monad (mapM)
import Data.ByteString (concat, readFile)
import Data.ByteString.Char8 (unpack)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (sort)
import Data.Monoid ((<>))
import System.Directory (listDirectory)
import System.IO (FilePath, IO)
import System.IO.Temp (writeSystemTempFile)

getMigrationFiles :: IO [FilePath]
getMigrationFiles = do
  let migrationDir :: FilePath = "test/migrations/"
  files <- listDirectory migrationDir
  pure . sort $ fmap (migrationDir <>) files

migrationFilesToSchemaFile :: [FilePath] -> IO FilePath
migrationFilesToSchemaFile files = do
  contents <- concat <$> mapM readFile files
  writeSystemTempFile "hasql-generator" (unpack contents)
