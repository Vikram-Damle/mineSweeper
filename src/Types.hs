module Types where

import Data.Array.IArray

data State = Visited | Unvisited
    deriving (Eq, Show)

data Content = Empty | Mine
    deriving (Eq, Show)

type Cell = (State, Content)

type CountCell = (State, Content, Int)

type Grid a = Array (Int, Int) a

type Coords = (Int, Int)