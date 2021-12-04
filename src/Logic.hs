module Logic where

import Types
import Utils

import Data.Array.IArray
import Data.List (foldl')


getEntries :: (IArray a e, Ix i) => a i e -> [i] -> [e]
getEntries = map . (!)

getNeighbours :: Grid Cell -> Coords -> ([Cell], [Coords])
getNeighbours world (h, w) = (getEntries world nbCrds, nbCrds)
    where
        ((l1, l2), (h1, h2)) = bounds world
        top     = if h < h1 then h+1 else -1
        bottom  = if h > l1 then h-1 else -1
        right   = if w < h2 then w+1 else -1
        left    = if w > l2 then w-1 else -1
        nbCrds  = [(a, b) | a <- [top, h, bottom] , b <- [left, w, right], not ((a==h) && (b==w)), a > 0, b > 0]


mineCount :: Grid Cell -> Coords -> Int
mineCount world (h, w) = foldl' mineCt 0 neighbours
    where
        ((l1, l2), (h1, h2)) = bounds world
        top     = min h1 (h+1)
        bottom  = max l1 (h-1)
        right   = min h2 (w+1)
        left    = max l2 (w-1)
        neighbours = fst $ getNeighbours  world (h, w)

        mineCt :: Int -> Cell -> Int
        mineCt acc (state, content)
            | content == Empty  = acc
            | otherwise         = 1 + acc


updateCell :: Grid Cell -> Coords -> Grid Cell
updateCell world p = world //  [(p, newCell)]
    where
        (oldState, oldContents) = world ! p
        newCell = (Visited, oldContents)


updateWorld :: Grid Cell -> Coords -> Grid Cell
updateWorld world p
    | state == Visited  = world
    | mineCt == 0       = updateNeighbours newWorld p
    | otherwise         = newWorld
    where
        mineCt = mineCount world p
        newWorld = updateCell world p
        (state, _) = world ! p


updateNeighbours :: Grid Cell -> Coords -> Grid Cell
updateNeighbours world p@(h, w) = updateNeighbours' world unbcoords
    where
        (neighbours, nbcoords) = getNeighbours world p
        unbcoords = map snd $ filter (\c -> (fst .fst) c == Unvisited) (zip neighbours nbcoords)
        updateNeighbours' :: Grid Cell -> [Coords] -> Grid Cell
        updateNeighbours' = foldl' updateWorld


cellToCountcell :: Grid Cell -> Grid CountCell
cellToCountcell world = array (bounds world) newWorldList
    where
        newWorldList = [(p, (fst c, snd c, mineCount world p)) | (p, c) <- assocs world]