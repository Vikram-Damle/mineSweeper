module Presentation where

import Types
import Utils
import Logic

import Data.Array.IArray


-- |Convert grid array to nest list
gridToList :: Grid a-> [[a]]
gridToList grid = nest width $ elems grid
    where
        ((l1, l2), (h1, h2)) = bounds grid
        width = h2-l2 + 1

        nest :: Int -> [a] -> [[a]]
        nest _ [] = [[]]
        nest n xs = take n xs : nest n (drop n xs)


-- |Convert cell to letter representation. Upper case == visited
toLetter :: CountCell -> String
toLetter (a, b, c)
    | cell == celle = "e"
    | cell == cellE = "E"
    | cell == cellm = "m"
    | cell == cellM = "M"
    | otherwise     = error "Undefined Cell" ++ show cell
    where
        cell = (a, b)


-- |convert cell to mine count
toNumber :: CountCell -> String
toNumber = show . third


-- |convert cell to mine count if visited else hidden with '*'. Visited mine = '#' (BOOM)
toHidden :: CountCell -> String
toHidden (state, contents, count)
    | state == Visited && contents == Empty = show count
    | state == Visited && contents == Mine  = "#"
    | otherwise         = "*"


-- |Add pipes and lines to edges of the grid display
format :: String -> String
format str = unlines $ [horz] ++ ["| " ++ line ++ " |" | line <- par, line /= ""] ++ [horz]
    where
        par = lines str
        horz = map (const '=') ("1234" ++ head par)


-- |Convert a Grid Cell to its final string representation to be printed directly to console
worldToString :: Grid Cell -> String
worldToString = format . unlines . map (unwords . map toHidden) . gridToList . cellToCountcell


