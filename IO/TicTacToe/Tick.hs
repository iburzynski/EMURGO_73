module Tick where

data Tick = X | O | E deriving (Eq, Read, Show)

flipTick :: Tick -> Tick
flipTick X = O
flipTick O = X
flipTick E = E

showTick :: Tick -> String
showTick X = "X"
showTick O = "O"
showTick E = "_"