module FeedIO (downloadFeed) where

import Data.Aeson
import Network.HTTP.Conduit (simpleHttp)
import System.IO.Error

import Feed

downloadFeed :: Int -> IO (Maybe Feed)
downloadFeed currentId = do
  let url = getUrl currentId
  putStrLn $ "Fetching the feed from the url " ++ url
  r <- tryIOError $ simpleHttp url
  case r of
    Left _  -> return Nothing
    Right f -> return $ decode f

getUrl :: Int -> String
getUrl n
    | n <= 0 = "http://xkcd.com/info.0.json"
    | otherwise = "http://xkcd.com/" ++ show n ++ "/info.0.json"
