module Main where

import Control.Exception
import Control.Monad.State
import Data.Char
import Data.List
import System.Random

data GameStatus = GameStatus { tries :: Int, triedLetters :: String, playerWord :: String, hiddenWord :: String }

main :: IO ()
main =
    do result <- try (readFile "words.txt") :: IO (Either SomeException String)
       case result of
         Left ex -> putStrLn $ "Caught exception: " ++ show ex
         Right content ->
           do r <- newStdGen
              let allWords = lines content
              let word = allWords !! fst (randomR (0, length allWords - 1) r)
              let initialStatus = GameStatus { tries = 2 * length (nub word)
                                               , triedLetters = []
                                               , playerWord = [1..(length word)] *> "."
                                               , hiddenWord = word }
              runStateT loopGame initialStatus >>= putStrLn . fst

loopGame :: StateT GameStatus IO String
loopGame =
    do currentStatus <- get
       case currentStatus of
         GameStatus t tl pw hw
           | pw == hw  -> return $ "You won!!! :) The word is: " ++ show hw
           | t == 0    -> return $ "Sorry you have lost :( The word is: " ++ show hw
           | otherwise -> do lift $ putStrLn $ "<< Left attempts " ++ show t ++ " -- " ++ show pw ++ " -- [ tried: " ++ tl ++ " ] >>"
                             guess <- lift gameInput
                             if length guess == 1 then
                               do let charGuessed = head guess
                                  if charGuessed `elem` tl then lift (putStrLn "You already have tried this letter")
                                  else put (currentStatus { tries = t - 1, triedLetters = sort $ nub (charGuessed:tl), playerWord = guessedWord pw hw charGuessed })
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
