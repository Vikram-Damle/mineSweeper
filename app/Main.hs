module Main where


-- import Data.Array.IArray

import Lib


main :: IO ()
main = do
    seed <- randInt
    let world = genWorld mineProb seed p
    instructions p
    run world


run :: Grid Cell -> IO ()
run currWorld = do
    putStrLn . worldToString $ currWorld
    p <- getCoords
    let (newWorld, boom) = runMove currWorld p
    if boom 
        then do
            putStrLn . worldToString $ newWorld
            putStrLn $ "Game Over. Mine exploded at " ++ show p
        else if winCondition newWorld
            then putStrLn "You Win! All mines found"
            else run newWorld

