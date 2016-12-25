module Main where

import Control.Exception
import Control.Monad.State
import Data.Char
import Data.List
import System.Random

data GameStatus = GameStatus { tries :: Int, playerWord :: String, hiddenWord :: String }

main :: IO ()
main =
    do result <- try (readFile "words.txt") :: IO (Either SomeException String)
       case result of
         Left ex -> putStrLn $ "Caught exception: " ++ show ex
         Right content -> startGame $ lines content


startGame :: [String] -> IO ()
startGame allWords = pickRandomWord allWords >>= \word -> runStateT loopGame (initNewGame word) >>= putStrLn . fst


pickRandomWord :: [String] -> IO String
pickRandomWord allWords = newStdGen >>= \r -> return $ allWords !! fst (randomR (0, length allWords - 1) r)


initNewGame :: String -> GameStatus
initNewGame word = GameStatus { tries = 2 * length (nub word), playerWord = [1..(length word)] *> "." , hiddenWord = word }


loopGame :: StateT GameStatus IO String
loopGame =
    do currentStatus <- get
       case currentStatus of
         GameStatus t pw hw
           | pw == hw  -> return $ "You won!!! :) The word is: " ++ show hw
           | t == 0    -> return $ "Sorry you have lost :( The word is: " ++ show hw
           | otherwise -> do lift $ putStrLn $ "<< Left attempts " ++ show t ++ " -- " ++ show pw ++ " >>"
                             guess <- lift gameInput
                             if length guess == 1 then
                               if head guess `elem` pw then lift (putStrLn "You already have guessed this letter")
                               else put (currentStatus { tries = t - 1, playerWord = guessedWord pw hw (head guess) })
                             else put (currentStatus { tries = t - 1 })
                             loopGame
       where
         guessedWord :: String -> String -> Char -> String
         guessedWord pw hw letter = foldr (\w acc -> (if snd w == letter then letter else fst w):acc) [] (pw `zip` hw)

         gameInput :: IO String
         gameInput =
             do putStr "Insert a new letter or enter full word >>> "
                getLine >>= parse
                  where
                    parse input
                      | not (null input) && all isAlpha input = return input
                      | otherwise = putStrLn "The input cannot be empty and only letters are accepted" >> gameInput
