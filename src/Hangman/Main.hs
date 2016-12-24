module Main where

import Control.Exception
import Control.Monad.State
import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.Sequence (fromList, elemIndicesL, update, Seq)
import System.Random

type SeqString = Seq Char

data GameResult = Continue | Discard | Lost | Repeat | Won
data GameStatus = InvalidSt
                  | ThrowableSt { tries :: Int, playerWord :: SeqString, hiddenWord :: SeqString }
                  | UpdatableSt { tries :: Int, playerWord :: SeqString, hiddenWord :: SeqString }

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
    do result <- try (readFile "../../words.txt") :: IO (Either SomeException String)
       case result of
         Left ex -> putStrLn $ "Caught exception: " ++ show ex
         Right content -> startGame $ lines content


startGame :: [String] -> IO ()
startGame allWords = pickRandomWord allWords >>= \word -> runStateT loopGame (initNewGame word) >>= putStrLn . fst


pickRandomWord :: [String] -> IO String
pickRandomWord allWords = newStdGen >>= \r -> return $ allWords !! fst (randomR (0, length allWords - 1) r)


initNewGame :: String -> GameStatus
initNewGame word = UpdatableSt (calcNumberTries word) (createGuessBox word) (fromList word)
                   where calcNumberTries w = 2 * length (nub w)
                         createGuessBox w = fromList $ [1..(length w)] *> "."


loopGame :: StateT GameStatus IO String
loopGame =
    do currentStatus <- get
       let pw = playerWord currentStatus
       let hw = hiddenWord currentStatus
       let t = tries currentStatus
       let nt = t - 1

       lift $ putStrLn $ "<< Left attempts " ++ show t ++ " -- " ++ show (toList pw) ++ " >>"
       guess <- lift $ gameInput currentStatus

       let updatedStatus | length guess > 1    = ThrowableSt { tries = nt, hiddenWord = hw, playerWord = fromList guess }
                         | letter `notElem` pw = UpdatableSt { tries = nt, hiddenWord = hw, playerWord = newPlayerWord }
                         | otherwise           = InvalidSt
            where letter = head guess
                  newPlayerWord = foldl (\acc index -> update index letter acc) pw (letter `elemIndicesL` hw)

       case eval updatedStatus of
         Won      -> return $ "You won!!! :) The word is: " ++ show (toList hw)
         Lost     -> return $ "Sorry you have lost :( The word is: " ++ show (toList hw)
         Repeat   -> do lift $ putStrLn "You already have guessed this letter"
                        loopGame
         Discard  -> do put currentStatus { tries = nt}
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
