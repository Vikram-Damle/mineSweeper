module Utils where

import Types

(h, w) = (10, 10) :: (Int, Int)
p = (h, w) :: Coords
mineProb = 0.1
celle = (Unvisited, Empty)  :: Cell
cellE = (Visited, Empty)    :: Cell
cellm = (Unvisited, Mine)   :: Cell
cellM = (Visited, Mine)     :: Cell

third :: (a, b, c) -> c
third (_, _, x) = x

(.:) = (.) (.) (.) 
infixr 2 .:


