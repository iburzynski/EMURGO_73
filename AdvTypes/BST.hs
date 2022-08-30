{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
import Data.Foldable (toList, foldl')

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)
-- a Tree can contain data of any one type...
-- and is either Empty or a Node consisting of:
--   * a left subtree (type Tree a)
--   * a value (type a)
--   * a right subtree (type Tree a)

myTree =
  Node (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)) 4 (Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty))

--       4
--     /   \
--    2     6
--   / \   / \
--  1   3 5   7

-- Write a function that inserts a value into the appropriate position of a binary search tree
insert :: Ord a => a -> Tree a -> Tree a
-- Base case: insert a node into an empty tree
insert x Empty = Node Empty x Empty
-- Recursive case: insert a node into the appropriate subtree of a non-empty tree
--   * If the value exceeds the current node's value, insert it recursively into the right subtree
--   * If the value is less than or equal to the current node's value, insert into the left subtree
insert x (Node left y right)
  | x > y     = Node left y (insert x right)
  | otherwise = Node (insert x left) y right

-- Write a function that builds a binary search tree from a list of input values using the `insert`
--   function and a fold.
fromList :: Ord a => [a] -> Tree a
fromList xs = foldl' (flip insert) Empty xs

foldEx = [7, 8, 5, 6]

-- *** Challenge Exercise: "Folds within Folds" ***
-- Define a Foldable instance for your binary search tree by implementing `foldr`.
instance Foldable Tree where
  foldr _ acc Empty = acc
  foldr f acc (Node left x right) =
-- 1. Make a new accumulator value by applying the reducer `f` to:
--      a.) the value of the current node `x`, and
--      b.) the new accumulator produced by folding `f` over the current acc. and the right subtree
    let newAcc = f x (foldr f acc right)
-- 2. Then fold the left subtree with `f` and the new accumulator value from Step 1.
    in foldr f newAcc left
-- These two steps alternate and repeat until the entire tree has been folded into a single value.

test = fromList [7, 2, 1, 4, 0, 19, 8]
