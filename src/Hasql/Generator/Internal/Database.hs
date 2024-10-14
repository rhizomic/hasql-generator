module Hasql.Generator.Internal.Database
  ( runTmpPostgresWith,
    withDb,
    withPool,
    migrate,
  )
where

import Control.Applicative (pure)
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (Bool (False, True))
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Char8 (hGetLine, pack, unwords)
import Data.Foldable (traverse_)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import GHC.Base (error)
import GHC.IO.Handle (Handle)
import GHC.Show (show)
import Hasql.Connection (Settings)
import Hasql.Generator.Internal.Database.Transaction (paramAndResultlessTransaction)
import Hasql.Generator.Internal.Database.Types
  ( DatabaseSettings
      ( DatabaseSettings,
        databaseName,
        host,
        port,
        user
      ),
  )
import Hasql.Pool (Pool, acquire, release)
import Hasql.Pool.Config (settings, staticConnectionSettings)
import Hasql.Transaction (Transaction)
import System.IO (IO)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
  ( ExitCode (ExitFailure, ExitSuccess),
    Process,
    createPipe,
    getStdout,
    readProcess,
    setStderr,
    setStdout,
    shell,
    startProcess,
    stopProcess,
  )

-- | Creates a temporary Postgres instance and runs an action against it.
runTmpPostgresWith ::
  forall a.
  (DatabaseSettings -> IO a) ->
  IO a
runTmpPostgresWith action = do
  user <- whoami

  withSystemTempDirectory "pg" $ \tmpDir -> do
    let cmd :: String = "tmp-postgres " <> tmpDir
        databaseSettings =
          DatabaseSettings
            { host = pack tmpDir
            , port = "5432"
            , user = user
            , databaseName = user
            }

    process <- startProcess (setStdout createPipe . setStderr createPipe $ shell cmd)
    runActionWhenReady process databaseSettings
  where
    runActionWhenReady ::
      Process stdin Handle stderr ->
      DatabaseSettings ->
      IO a
    runActionWhenReady process databaseSettings = do
      stdout <- fmap decodeUtf8 <$> hGetLine $ getStdout process
      -- We can only connect to the DB (and run queries) after `createdb` is
      -- finished.
      let ready = "createdb terminated with exit status: 0" `isInfixOf` stdout

      case ready of
        True -> do
          result <- action databaseSettings
          stopProcess process
          pure result
        False -> runActionWhenReady process databaseSettings

    whoami ::
      (MonadIO m) =>
      m ByteString
    whoami = do
      let cmd :: String = "whoami"
      (exitCode, stdOut, stdErr) <- readProcess $ shell cmd
      case exitCode of
        ExitSuccess -> pure $ toStrict stdOut
        ExitFailure _failureCode -> error (show stdErr)

-- | Runs an action using the connection pool for the database specified in
--   'DatabaseSettings'.
withPool ::
  DatabaseSettings ->
  (Pool -> IO a) ->
  IO a
withPool databaseSettings action =
  let hasqlSettings = databaseSettingsToHasqlSettings databaseSettings
      poolConfig = settings [staticConnectionSettings hasqlSettings]
   in bracket (acquire poolConfig) release action
  where
    databaseSettingsToHasqlSettings ::
      DatabaseSettings ->
      Settings
    databaseSettingsToHasqlSettings dbSettings =
      unwords
        [ setting "host" dbSettings.host
        , setting "port" dbSettings.port
        , setting "user" dbSettings.user
        , setting "dbname" dbSettings.databaseName
        ]
      where
        setting :: ByteString -> ByteString -> ByteString
        setting label value = label <> "=" <> value

-- | Runs an action using the connection pool for a temporary database.
withDb ::
  (Pool -> IO a) ->
  IO a
withDb action =
  runTmpPostgresWith $ \dbSettings ->
    withPool dbSettings $ \pool ->
      action pool

-- | Migrates a series of migrations (in the order that they were provided).
migrate :: [ByteString] -> Transaction ()
migrate = traverse_ paramAndResultlessTransaction
