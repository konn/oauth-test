{-# LANGUAGE OverloadedStrings #-}
module TwitterSettings where
import Web.Authenticate.OAuth
import Data.ByteString.Char8

consumerKey :: String
consumerKey = ""

consumerSecret :: String
consumerSecret = ""

twitter :: OAuth
twitter = OAuth { oauthServerName = "twitter"
                , oauthRequestUri = "http://api.twitter.com/oauth/request_token"
                , oauthAccessTokenUri = "http://api.twitter.com/oauth/access_token"
                , oauthAuthorizeUri = "http://api.twitter.com/oauth/authorize"
                , oauthSignatureMethod = HMACSHA1
                , oauthConsumerKey = pack consumerKey
                , oauthConsumerSecret = pack consumerSecret
                , oauthCallback = Nothing
                }
