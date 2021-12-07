module Generate
    ( randInt
    , genWorld
    ) where

import Types
import Utils

import System.Random
import Data.Array


randInt = randomIO :: IO Int


-- |Generate an infinite list of bools with True having probability p
genBools :: Double -> Int -> [Bool]
genBools prob seed = map (< prob) (randoms (mkStdGen seed) :: [Double])


-- |Convert from a list of bools (of sufficient length) to a world grid of given size with True meaning Mine 
boolsToWorld :: [Bool] -> Coords -> Grid Cell
boolsToWorld bs p = listArray ((1, 1), p)  [(Unvisited, if b then Mine else Empty) | b <- bs]


genWorld :: Double -> Int -> Coords -> Grid Cell
genWorld = boolsToWorld .: genBools  