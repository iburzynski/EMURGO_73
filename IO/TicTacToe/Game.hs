module Game where

import Tick
import Data.List (transpose)
import Text.Read (readMaybe)
import Data.Char (toUpper)

type Size     = Int
type Row      = [Tick]
type Squares  = [Row]
type ColIdx   = Int
type Position = (Int, Int)

data Board = Board { getSize    :: Int
                   , getSquares :: Squares
                   }

makeBoard :: Size -> Board
makeBoard s = Board { getSize = s
                    , getSquares = replicate s (replicate s E)
                    }
-- makeBoard 3 == [[E, E, E], [E, E, E], [E, E, E]]

getTick :: Position -> Squares -> Tick
getTick (i, j) = (!! j) . (!! i)
-- getTick (i, j) xs = xs !! i !! j

validateMove :: Board -> String -> Maybe Position
-- a valid move string is exactly two characters and has format "<row letter><col. number>"
validateMove b [i, j] = mj >>= checkIndices
  where
    mj = readMaybe [j] :: Maybe Int -- check if 2nd char. is a number
    i' = subtract 65 . fromEnum $ toUpper i -- convert 1st char. to its Unicode digit & offset by -65
    inBounds :: Int -> Bool
    inBounds = (< getSize b) -- check if an index is within the board size
    isEmpty :: (Int, Int) -> Bool
    isEmpty = (== E) . flip getTick (getSquares b) -- check if the square at the position is empty
    -- isEmpty pos = getTick pos (getSquares b) == E
    checkIndices :: Int -> Maybe Position
    checkIndices j'
      | all inBounds [i', j'] && isEmpty (i', j') = Just (i', j') -- return a valid position if all validation predicates are satisfied
      | otherwise = Nothing

validateMove _ _      = Nothing -- any string that isn't two characters is invalid

playMove :: Board -> Tick -> Position -> Board
playMove b t p = b { getSquares = go t p sqs } -- return a new board with updated squares
  where
    sqs = getSquares b
    -- recursive helper function to generate new list of squares
    go :: Tick -> Position -> Squares -> Squares
    go _ _      []           = []
    -- if position is in current row, add tick at specified column index and cons to remaining rows
    go t (0, j) (row : rows) = addTick t j row : rows
    -- otherwise, preserve current row and repeat on remaining rows, decrementing row index
    go t (i, j) (row : rows) = row : go t (i - 1, j) rows
    -- generates a new row with a tick at the specified column index
    addTick :: Tick -> ColIdx -> Row -> Row
    addTick _ _ []       = []
    addTick t 0 (_ : xs) = t : xs -- if index is 0, change head to tick and cons to tail
    -- otherwise, preserve head and repeat on tail, decrementing col. index
    addTick t j (x : xs) = x : addTick t (j - 1) xs

gameOver :: Board -> (Bool, Tick)
gameOver b
  | hasWon b X    = (True, X)  -- check if X has won
  | hasWon b O    = (True, O)  -- check if O has won
  | isBoardFull b = (True, E)  -- check if tied
  | otherwise     = (False, E) -- game is still going

hasWon :: Board -> Tick -> Bool
hasWon b t = any (($ getSquares b) . ($ t)) [horizMate, vertMate, diagMate]
--                                   (\func -> func $ t)
--                                          ^ create a list of predicate functions
--                                   ^ partially apply `t` as the first argument to all of them
--                 ^ apply the board squares as the second argument to all of them
--           ^ we now have a list of Bools; return True if any of them are True (player has won)

isBoardFull :: Board -> Bool
-- check if `E` doesn't exist anywhere in the board:
isBoardFull = (E `notElem`) . concat . getSquares
--            (\xs -> notElem E xs)

horizMate :: Tick -> Squares -> Bool
horizMate _ []           = False
-- check if all elements in the current row are the given tick,
-- or in the next row, etc.
horizMate t (row : rows) = all (== t) row || horizMate t rows

vertMate :: Tick -> Squares -> Bool
-- transpose the board (flip rows and columns) and check if any row is a mate
vertMate t rows = horizMate t (transpose rows)

diagMate :: Tick -> Squares -> Bool
diagMate t rows = horizMate t [trace rows, antitrace rows]
  where
    -- get the diagonal row from top left to bottom right corners
    trace :: Squares -> Row
    trace []           = []
    trace (row : rows) = head row : trace (map tail rows)
    -- 1. takes first elem of the leading row (`head`)
    -- 2. drops the leading column of the entire board (`map tail`)
    -- 3. repeat with next row

    -- get the diagonal row from top right to bottom left corners
    antitrace :: Squares -> Row
    antitrace []           = []
    antitrace (row : rows) = last row : antitrace (map init rows)