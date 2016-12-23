module Main where

import Control.Exception
import Control.Monad.State
import Data.Char
import Data.Either
import Data.Foldable (toList)
import Data.List
import Data.Sequence (fromList, elemIndicesL, update, Seq)
import System.Random

import Lib

type SeqString = Seq Char

data GameResult = Continue | Discard | Lost | Repeat | Won
data GameStatus = InvalidSt
                  | ThrowableSt { tries :: Int, playerWord :: SeqString, hiddenWord :: SeqString }
                  | UpdatableSt { tries :: Int, playerWord :: SeqString, hiddenWord :: SeqString }

class GameResultEval a where
    eval :: a -> GameResult

instance GameResultEval GameStatus where
    eval (UpdatableSt tries playerWord hiddenWord)
        | playerWord == hiddenWord = Won
        | tries == 0 = Lost
        | otherwise = Continue

    eval (ThrowableSt tries playerWord hiddenWord)
        | playerWord == hiddenWord = Won
        | tries == 0 = Lost
        | otherwise = Discard

    eval InvalidSt = Repeat


main :: IO ()
main =
    do result <- try (readFile "words.txt") :: IO (Either SomeException String)
       case result of
         Left ex -> putStrLn $ "Caught exception: " ++ show ex
         Right content -> startGame $ lines content


startGame :: [String] -> IO ()
startGame words = pickRandomWord words >>= \word -> runStateT loopGame (initNewGame word) >>= putStrLn . fst


pickRandomWord :: [String] -> IO String
pickRandomWord words = newStdGen >>= \r -> return $ words !! fst (randomR (0, length words - 1) r)


initNewGame :: String -> GameStatus
initNewGame word = UpdatableSt (calcNumberTries word) (createGuessBox word) (fromList word)
                   where calcNumberTries word = 2 * length (nub word)
                         createGuessBox word = fromList $ [1..(length word)] *> "."


loopGame :: StateT GameStatus IO String
loopGame =
    do currentStatus <- get
       lift $ putStrLn $ "<< Left attempts " ++ show (tries currentStatus) ++ " -- " ++ show (toList (playerWord currentStatus)) ++ " >>"
       guess <- lift $ gameInput currentStatus

       let updatedStatus | length guess > 1                           = ThrowableSt { tries = tries currentStatus - 1, playerWord = fromList guess, hiddenWord = hiddenWord currentStatus }
                         | head guess `elem` playerWord currentStatus = InvalidSt
                         | otherwise                                  = newStatus currentStatus (head guess)

       case eval updatedStatus of
         Won      -> return $ "You won!!! :) The word is: " ++ show (toList (hiddenWord updatedStatus))
         Lost     -> return $ "Sorry you have lost :( The word is: " ++ show (toList (hiddenWord updatedStatus))
         Repeat   -> do lift $ putStrLn "You already have guessed this letter"
                        loopGame
         Discard  -> do put currentStatus { tries = tries currentStatus - 1}
                        loopGame
         Continue -> do put updatedStatus
                        loopGame


gameInput :: GameStatus -> IO String
gameInput status =
    do putStr "Insert a new letter or enter full word >>> "
       input <- getLine
       if not (null input) && all isAlpha input
         then return input
       else
         do putStrLn "The input cannot be empty and only letters are accepted"
            gameInput status


newStatus :: GameStatus -> Char -> GameStatus
newStatus currentStatus guessedLetter =
    do let hw = hiddenWord currentStatus
       let pw = playerWord currentStatus
       let nt = tries currentStatus - 1
       let ids = guessedLetter `elemIndicesL` hw
       case ids of
         (x:xs) -> let updateWord = foldl (\acc id -> update id guessedLetter acc) pw ids
                   in UpdatableSt { tries = nt, playerWord = updateWord, hiddenWord = hw }
         _      -> UpdatableSt { tries = nt, playerWord = pw, hiddenWord = hw  }
