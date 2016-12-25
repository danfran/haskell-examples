module Main where

import Control.Exception
import Control.Monad.State
import Data.Char
import Data.List
import System.Random

data GameResult = Continue | Discard | Lost | Repeat | Won
data GameStatus = InvalidSt
                  | ThrowableSt { tries :: Int, playerWord :: String, hiddenWord :: String }
                  | UpdatableSt { tries :: Int, playerWord :: String, hiddenWord :: String }

class GameResultEval a where
    eval :: a -> GameResult

instance GameResultEval GameStatus where
    eval (UpdatableSt t pw hw)
        | pw == hw = Won
        | t == 0 = Lost
        | otherwise = Continue

    eval (ThrowableSt t pw hw)
        | pw == hw = Won
        | t == 0 = Lost
        | otherwise = Discard

    eval InvalidSt = Repeat


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
initNewGame word = UpdatableSt { tries = 2 * length (nub word)
                                 , playerWord = [1..(length word)] *> "."
                                 , hiddenWord = word
                               }


loopGame :: StateT GameStatus IO String
loopGame =
    do currentStatus <- get
       let pw = playerWord currentStatus
       let hw = hiddenWord currentStatus
       let t = tries currentStatus
       let nt = t - 1

       lift $ putStrLn $ "<< Left attempts " ++ show t ++ " -- " ++ show pw ++ " >>"
       guess <- lift gameInput

       let newStatus  | length guess > 1    = ThrowableSt { tries = nt, hiddenWord = hw, playerWord = guess }
                      | letter `notElem` pw = UpdatableSt { tries = nt, hiddenWord = hw, playerWord = newPlayerWord }
                      | otherwise           = InvalidSt
                      where letter = head guess
                            newPlayerWord = foldr (\w acc -> (if snd w == letter then letter else fst w):acc) [] (pw `zip` hw)

       case eval newStatus of
         Continue -> put newStatus >> loopGame
         Discard  -> put currentStatus { tries = nt} >> loopGame
         Repeat   -> lift (putStrLn "You already have guessed this letter") >> loopGame
         Lost     -> return $ "Sorry you have lost :( The word is: " ++ show hw
         Won      -> return $ "You won!!! :) The word is: " ++ show hw

       where
         gameInput :: IO String
         gameInput =
             do putStr "Insert a new letter or enter full word >>> "
                getLine >>= parse
                  where
                    parse input
                      | not (null input) && all isAlpha input = return input
                      | otherwise = putStrLn "The input cannot be empty and only letters are accepted" >> gameInput
