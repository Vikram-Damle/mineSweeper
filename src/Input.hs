module Input where


getCoords :: IO (Int, Int)
getCoords = do
    putStrLn "Enter coordinates (h, w): "
    line <- words <$> getLine
    let (h, w) = (head line , head.tail $ line)
    return (read h :: Int, read w :: Int)


instructions :: (Int, Int) -> IO ()
instructions (h, w) = do
    putStrLn "For each move, enter coordinates of the cell to explore"
    putStrLn $ "The bottom left of the grid is (" ++ show h ++ ", 0) and top right is (0, "++ show w++")"

