module Presentation where

import Types
import Utils
import Logic

import Data.Array.IArray

gridToList :: Grid a-> [[a]]
gridToList grid = nest width $ elems grid
    where
        ((l1, l2), (h1, h2)) = bounds grid
        width = h2-l2 + 1

        nest :: Int -> [a] -> [[a]]
        nest _ [] = [[]]
        nest n xs = take n xs : nest n (drop n xs)


toLetter :: CountCell -> String
toLetter (a, b, c)
    | cell == celle = "e"
    | cell == cellE = "E"
    | cell == cellm = "m"
    | cell == cellM = "M"
    | otherwise     = error "Undefined Cell" ++ show cell
    where
        cell = (a, b)


toNumber :: CountCell -> String
toNumber = show . third


toHidden :: CountCell -> String
toHidden (state, contents, count)
    | state == Visited && contents == Empty = show count
    | state == Visited && contents == Mine  = "#"
    | otherwise         = "*"


format :: String -> String
format str = unlines $ [horz] ++ ["| " ++ line ++ " |" | line <- par, line /= ""] ++ [horz]
    where
        par = lines str
        horz = map (const '=') ("1234" ++ head par)


worldToString :: Grid Cell -> String
worldToString = format . unlines . map (unwords . map toHidden) . gridToList . cellToCountcell


