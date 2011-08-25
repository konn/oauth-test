{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, TypeSynonymInstances #-}
module Foundation
    ( Testing (..)
    , TestingRoute (..)
    , resourcesTesting
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    ) where

import Yesod
import Yesod.Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.OAuth
import Yesod.Logger (Logger, logLazyText)
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, luciusFile, juliusFile, widgetFile)
import Model
import Data.Maybe (isJust)
import Control.Monad (join, unless, mzero, (<=<))
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import qualified Data.Text as T
import Data.Text.Encoding
import Web.ClientSession (getKey)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Control.Applicative
import Control.Arrow
import Network.HTTP.Enumerator
import Data.Aeson
import Web.Authenticate.OAuth hiding (insert)
import Data.Attoparsec.Lazy
import Data.String
import qualified Data.Map as M

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Testing = Testing
    { settings :: Settings.AppConfig
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype TestingRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Testing = TestingRoute
-- * Creates the value resourcesTesting which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Testing. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the TestingRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Testing" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Testing where
    approot = Settings.appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogMessage loc level msg >>= logLazyText (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticDir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])


-- How to run database actions.
instance YesodPersist Testing where
    type YesodPersistBackend Testing = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= Settings.runConnectionPool f

instance YesodAuth Testing where
    type AuthId Testing = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        let twCredential = Credential . map (encodeUtf8 *** encodeUtf8) $ credsExtra creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
              musr <- liftIO $ do
                req <- parseUrl $ "http://api.twitter.com/1/users/show.json?screen_name=" ++ (T.unpack $ credsIdent creds)
                src <- responseBody <$> (withManager . httpLbs =<< signOAuth Settings.twitter twCredential req)
                let info = credsExtra creds
                    ans = look "screen_name" =<< maybeResult (parse json src)
                return $ User <$> ans <*> (T.unpack <$> lookup "oauth_token" info)
                                      <*> (T.unpack <$> lookup "oauth_token_secret" info)
              case musr of
                Nothing  -> return Nothing
                Just usr -> Just <$> insert usr


    authPlugins = [ authTwitter Settings.consumerKey Settings.consumerSecret
                  ]

look :: FromJSON a => T.Text -> Value -> Maybe a
look key = fromJSON' <=< M.lookup key <=< fromJSON'

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

instance FromJSON User where
  parseJSON (Object v) = User <$> v .: "screen_name" <*> pure undefined <*> pure undefined
  parseJSON _          = mzero

instance RenderMessage Testing FormMessage where
    renderMessage _ _ = defaultFormMessage