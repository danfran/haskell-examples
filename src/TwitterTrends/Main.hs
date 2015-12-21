{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import Data.Text (Text, unpack)
import qualified Data.ByteString.Char8 as BS
import GHC.Generics
import System.Environment

data Trends =
  Trends { trends :: [Trend]
         } deriving (Show, Generic)

data Trend =
  Trend {  tweet_volume :: Maybe(Integer)
           , name :: !Text
        } deriving (Show, Generic)

instance FromJSON Trends
instance ToJSON Trends

instance FromJSON Trend
instance ToJSON Trend

trendsUrl :: String -> String
trendsUrl woeid = "https://api.twitter.com/1.1/trends/place.json?id=" ++ woeid

getTrends :: OAuth -> Credential -> String -> IO (Either String [Trends])
getTrends appOauth appCred url = do
    request <- parseUrl url
    signedRequest <- signOAuth appOauth appCred request
    manager <- newManager tlsManagerSettings
    result <- httpLbs signedRequest manager
    return $ eitherDecode $ responseBody result

trendToText :: Trend -> String
trendToText trend = unpack ( name trend ) ++ " >>> " ++ volume
                    where volume = case tweet_volume trend of
                                     Nothing -> "no volume"
                                     Just v -> "volume " ++ show v

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ woeid, newOauthConsumerKey, newOauthConsumerSecret, newCredentialKey, newCredentialKeySecret ] -> do

      t <- getTrends appOauth appCred url

      case t of
        Left err -> putStrLn err
        Right listOfTrends -> do let ts = trends $ listOfTrends !! 0
                                 mapM_ putStrLn ( trendToText <$> ts )

      where
        url = trendsUrl woeid

        appOauth = newOAuth { oauthServerName       = "api.twitter.com"
                              , oauthConsumerKey    = BS.pack newOauthConsumerKey
                              , oauthConsumerSecret = BS.pack newOauthConsumerSecret
                            }

        appCred = newCredential ( BS.pack newCredentialKey ) ( BS.pack newCredentialKeySecret )

    _ -> putStrLn "Usage: TwitterTrends <WOEID> <oauthConsumerKey> <oauthConsumerSecret> <credentialKey> <credentialKeySecret>"
