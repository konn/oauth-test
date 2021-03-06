{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , luciusFile
    , textFile
    , widgetFile
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , staticRoot
    , staticDir
    , loadConfig
    , AppEnvironment(..)
    , AppConfig(..)
    , module TwitterSettings
    ) where

import qualified Text.Hamlet as S
import qualified Text.Cassius as S
import qualified Text.Julius as S
import qualified Text.Lucius as S
import qualified Text.Shakespeare.Text as S
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite

import Yesod (liftIO, MonadControlIO, addWidget, addCassius, addJulius, addLucius, whamletFile)
import Data.Monoid (mempty)
import System.Directory (doesFileExist)
import Data.Text (Text, pack)
import Data.Object
import qualified Data.Object.Yaml as YAML
import Control.Monad (join)
import TwitterSettings

data AppEnvironment = Test
                    | Development
                    | Staging
                    | Production
                    deriving (Eq, Show, Read, Enum, Bounded)

-- | Dynamic per-environment configuration loaded from the YAML file Settings.yaml.
-- Use dynamic settings to avoid the need to re-compile the application (between staging and production environments).
--
-- By convention these settings should be overwritten by any command line arguments.
-- See config/Foundation.hs for command line arguments
-- Command line arguments provide some convenience but are also required for hosting situations where a setting is read from the environment (appPort on Heroku).
--
data AppConfig = AppConfig {
    appEnv :: AppEnvironment

  , appPort :: Int

    -- | Your application will keep a connection pool and take connections from
    -- there as necessary instead of continually creating new connections. This
    -- value gives the maximum number of connections to be open at a given time.
    -- If your application requests a connection when all connections are in
    -- use, that request will fail. Try to choose a number that will work well
    -- with the system resources available to you while providing enough
    -- connections for your expected load.
    --
    -- Connections are returned to the pool as quickly as possible by
    -- Yesod to avoid resource exhaustion. A connection is only considered in
    -- use while within a call to runDB.
  , connectionPoolSize :: Int

    -- | The base URL for your application. This will usually be different for
    -- development and production. Yesod automatically constructs URLs for you,
    -- so this value must be accurate to create valid links.
    -- Please note that there is no trailing slash.
    --
    -- You probably want to change this! If your domain name was "yesod.com",
    -- you would probably want it to be:
    -- > "http://yesod.com"
  , appRoot :: Text
} deriving (Show)

loadConfig :: AppEnvironment -> IO AppConfig
loadConfig env = do
    allSettings <- (join $ YAML.decodeFile ("config/settings.yml" :: String)) >>= fromMapping
    settings <- lookupMapping (show env) allSettings
    portS <- lookupScalar "port" settings
    hostS <- lookupScalar "host" settings
    connectionPoolSizeS <- lookupScalar "connectionPoolSize" settings
    return $ AppConfig {
      appEnv = env
    , appPort = read portS
    , appRoot = pack (hostS ++ ":" ++ portS)
    , connectionPoolSize = read connectionPoolSizeS
    }

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig ->  Text
staticRoot conf = [st|#{appRoot conf}/static|]


-- The rest of this file contains settings which rarely need changing by a
-- user.

-- The next functions are for allocating a connection pool and running
-- database actions using a pool, respectively. They are used internally
-- by the scaffolded application, and therefore you will rarely need to use
-- them yourself.
runConnectionPool :: MonadControlIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

withConnectionPool :: MonadControlIO m => AppConfig -> (ConnectionPool -> m a) -> m a
withConnectionPool conf f = do
    cs <- liftIO $ loadConnStr (appEnv conf)
    withSqlitePool cs (connectionPoolSize conf) f
  where
    -- | The database connection string. The meaning of this string is backend-
    -- specific.
    loadConnStr :: AppEnvironment -> IO Text
    loadConnStr env = do
        allSettings <- (join $ YAML.decodeFile ("config/sqlite.yml" :: String)) >>= fromMapping
        settings <- lookupMapping (show env) allSettings
        lookupScalar "database" settings

-- Example of making a dynamic configuration static
-- use /return $(mkConnStr Production)/ instead of loadConnStr
-- mkConnStr :: AppEnvironment -> Q Exp
-- mkConnStr env = qRunIO (loadConnStr env) >>= return . LitE . StringL


-- The following functions are used for calling HTML, CSS,
-- Javascript, and plain text templates from your Haskell code. During development,
-- the "Debug" versions of these functions are used so that changes to
-- the templates are immediately reflected in an already running
-- application. When making a production compile, the non-debug version
-- is used for increased performance.
--
-- You can see an example of how to call these functions in Handler/Root.hs
--
-- Note: due to polymorphic Hamlet templates, hamletFileDebug is no longer
-- used; to get the same auto-loading effect, it is recommended that you
-- use the devel server.

-- | expects a root folder for each type, e.g: hamlet/ lucius/ julius/
globFile :: String -> String -> FilePath
globFile kind x = kind ++ "/" ++ x ++ "." ++ kind

hamletFile :: FilePath -> Q Exp
hamletFile = S.hamletFile . globFile "hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile = 
#ifdef PRODUCTION
  S.cassiusFile . globFile "cassius"
#else
  S.cassiusFileDebug . globFile "cassius"
#endif

luciusFile :: FilePath -> Q Exp
luciusFile = 
#ifdef PRODUCTION
  S.luciusFile . globFile "lucius"
#else
  S.luciusFileDebug . globFile "lucius"
#endif

juliusFile :: FilePath -> Q Exp
juliusFile =
#ifdef PRODUCTION
  S.juliusFile . globFile "julius"
#else
  S.juliusFileDebug . globFile "julius"
#endif

textFile :: FilePath -> Q Exp
textFile =
#ifdef PRODUCTION
  S.textFile . globFile "text"
#else
  S.textFileDebug . globFile "text"
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = whenExists (globFile "hamlet")  (whamletFile . globFile "hamlet")
    let c = whenExists (globFile "cassius") cassiusFile
    let j = whenExists (globFile "julius")  juliusFile
    let l = whenExists (globFile "lucius")  luciusFile
    [|addWidget $h >> addCassius $c >> addJulius $j >> addLucius $l|]
  where
    whenExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]