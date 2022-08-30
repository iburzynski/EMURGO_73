{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where

import Tick
import Game
import Data.List (intercalate)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Enter the size of the board (3 to 10):"
  ms <- getSize
  case ms of
    -- if a number isn't provided, print error and restart
    Nothing -> putStrLn "Enter a valid number" >> main
    Just s  -> if s >= 3 && s <= 10
      -- if a valid number is provided within the accepted range, initiate the game
      then play X (makeBoard s)
      -- otherwise print error and restart
      else putStrLn "Invalid board size" >> main
  where
    getSize :: IO (Maybe Int)
    getSize = do
      s <- getLine
      return $ readMaybe s
    -- alternately: getSize = getLine   >>= return . readMaybe
    -- (>>=) ::               IO String ->  (String -> IO (Maybe Int))  -> IO b

play :: Tick -> Board -> IO ()
play t b = do
  showBoard b
  concatPrint [
      "Player "
    , show t
    , "'s turn: enter a row and column position (ex. A1)"
    ]
  move <- getLine -- get player's move
  let mp = validateMove b move
  case mp of
    Nothing -> putStrLn "Invalid position!" >> play t b -- if validation fails, print error & re-run
    Just p  -> let b' = playMove b t p in -- update the board state
          case gameOver b' of -- check if game is over
            (False, _)     -> play (flipTick t) b' -- if not, play again with opponent
            -- if over, print the relevant end of game message and end
            (True, E)      -> showBoard b' >> putStrLn "Players X and O have drawn the game."
            (True, player) -> showBoard b' >> concatPrint [ "Player "
                                                          , show player
                                                          , " has won the game."
                                                          ]
  where
    concatPrint = putStrLn . concat -- helper function for convenience

-- Prints the current representation of the board state to the terminal:
showBoard :: Board -> IO ()
showBoard b = mapM_ putStrLn allStrs
  where
    s       = getSize b
    sqs     = getSquares b
    -- prepare the header string with numbered columns up to the board width
    header  = "  | " ++ intercalate " | " (map show [0 .. s - 1]) ++ " |"
    -- combine header string with formatted board row strings prefixed by row letters
    allStrs = header : zipWith (:) (map (\x -> toEnum $ x + 65) [0 .. s - 1]) (map fmtRow sqs)
    -- creates a formatted string representation of the row
    fmtRow :: Row -> String
    fmtRow r = " | " ++ intercalate " | " (map showTick r) ++ " |"

