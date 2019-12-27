{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Char (toLower)
import Data.Map hiding (unions)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import Reactive.Banana hiding (First, Last)
import Reactive.Banana.Frameworks
import System.Random

import FeedIO
import Feed
import AppState

data Navigation = First | Previous | Random | Next | Last deriving (Eq)

navigationMap :: Map [Char] Navigation
navigationMap = fromList [("first", First), ("previous", Previous), ("random", Random), ("next", Next), ("last", Last)]

main :: IO ()
main = do
  lastId <- getNum . fromJust <$> downloadFeed 0
  (addInputHandler, fireEvent) <- newAddHandler
  appNetwork addInputHandler lastId

  forever $ do
    l <- getLine
    case (lookup (toLower <$> l) navigationMap) of
         Just command -> fireEvent command
         Nothing -> putStrLn "OOOPS! You can only use the commands `first`, `previous`, `random`, `next` and `last` :)"


appNetwork :: AddHandler Navigation -> Int -> IO ()
appNetwork addInputHandler lastId = do
  let networkDescription :: AddHandler Navigation -> MomentIO ()
      networkDescription inputHandler = do

          input <- fromAddHandler inputHandler

          fetchRandomIndex <- fromPoll $ getStdRandom (randomR (1, lastId))

          (navigationSelection :: Event Int)
              <- accumE lastId $
                  unions [
                      doFirst <$ filterE (\e -> e == First) input
                      , doPrevious <$ filterE (\e -> e == Previous) input
                      , doRandom <$> fetchRandomIndex <@ filterE (\e -> e == Random) input
                      , doNext lastId <$ filterE (\e -> e == Next) input
                      , doLast lastId <$ filterE (\e -> e == Last) input
                  ]

          fetchFeed2E <- mapEventIO downloadFeed navigationSelection
          reactimate $ displayInfo <$> fromJust <$> fetchFeed2E

  network <- compile $ networkDescription addInputHandler
  actuate network
  where
    displayInfo :: Feed -> IO ()
    displayInfo feed = do
      mapM_ putStrLn [
        replicate 50 '-'
        , "Issue #" ++ show (getNum feed) ++ " (" ++ getDate feed ++ ")"
        , "Title: " ++ getTitle feed ++ " - " ++ getAlt feed
        , "Image URI: " ++ getUri feed
        ]
