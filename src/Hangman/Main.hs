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
              fst <$> runStateT loopGame GameStatus { tries = 2 * length (nub word), triedLetters = []
                                                      , playerWord = [1..(length word)] *> ".", hiddenWord = word }

loopGame :: StateT GameStatus IO ()
loopGame =
    do currentStatus <- get
       case currentStatus of
         GameStatus t tl pw hw ->
            do printIO $ "<< Left attempts " ++ show t ++ " -- " ++ show pw ++ " -- [ tried: " ++ tl ++ " ] >>"
               guess <- lift gameInput
               case guess of
                [l] -> if l `elem` tl then printIO "You already have tried this letter" >> loopGame
                       else let gw = foldr (\w acc -> (if snd w == l then l else fst w):acc) [] (pw `zip` hw)
                            in evalNewGuess t gw (currentStatus { tries = t - 1, triedLetters = sort $ nub (l:tl), playerWord = gw })
                _   -> evalNewGuess t guess (currentStatus { tries = t - 1 })
               where
                 evalNewGuess :: Int -> String -> GameStatus -> StateT GameStatus IO ()
                 evalNewGuess tr gw status | hw == gw  = printIO $ "You won!!! :) The word is: " ++ show hw
                                           | tr == 1   = printIO $ "Sorry you have lost :( The word is: " ++ show hw
                                           | otherwise = put status >> loopGame
       where
         printIO = lift . putStrLn

         gameInput :: IO String
         gameInput =
             do putStr "Insert a new letter or enter full word >>> "
                getLine >>= parse
                  where
                    parse input
                      | not (null input) && all isAlpha input = return input
                      | otherwise = putStrLn "The input cannot be empty and only letters are accepted" >> gameInput
