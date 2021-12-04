module Main where


import Data.Array.IArray

import Lib
import Utils


main :: IO ()
main = do
    let world = listArray ((1,1), (h,w)) $ concat $ replicate (h*w) [celle, cellE, celle, cellm] :: Grid Cell
    putStrLn $  worldToString world
    return ()

