module Hasql.Generator.Internal.Database
  ( runTmpPostgresWith,
    withDb,
    withPool,
  )
where

import Control.Applicative (pure)
import Control.Exception (bracket)
import Data.Bool (Bool (False, True))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (hGetLine, pack, unwords)
import Data.Either (Either (Left, Right))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text, isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Handle (Handle)
import Hasql.Connection (Settings)
import Hasql.Generator.Internal.Database.Types
  ( DatabaseSettings
      ( DatabaseSettings,
        host
      ),
  )
import Hasql.Pool (Pool, acquire, release)
import Hasql.Pool.Config (settings, staticConnectionSettings)
import System.Directory (findExecutable)
import System.IO (IO)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
  ( Process,
    createPipe,
    getStdout,
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
  IO (Either Text a)
runTmpPostgresWith action = do
  mTmpPostgres <- findExecutable "tmp-postgres"

  case mTmpPostgres of
    Nothing ->
      pure $ Left "Could not locate tmp-postgres. Is it installed?"
    Just tmpPostgres ->
      withSystemTempDirectory "pg" $ \tmpDir -> do
        let cmd :: String = tmpPostgres <> " " <> tmpDir
            databaseSettings =
              DatabaseSettings
                { host = pack tmpDir
                }

        process <- startProcess (setStdout createPipe . setStderr createPipe $ shell cmd)
        Right <$> runActionWhenReady process databaseSettings
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
        ]
      where
        setting :: ByteString -> ByteString -> ByteString
        setting label value = label <> "=" <> value

-- | Runs an action using the connection pool for a temporary database.
withDb ::
  (DatabaseSettings -> Pool -> IO a) ->
  IO (Either Text a)
withDb action =
  runTmpPostgresWith $ \dbSettings ->
    withPool dbSettings $ \pool ->
      action dbSettings pool
