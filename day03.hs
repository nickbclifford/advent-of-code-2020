type Grid = [[Char]]

isTree :: Grid -> Int -> Int -> Integer
isTree grid row col =
    let rowList = grid !! row
    in case rowList !! (col `mod` length rowList) of -- mod to handle the "repeating"
        '.' -> 0
        '#' -> 1

stepCount :: Grid -> Int -> Int -> Int -> Int -> Integer
stepCount grid down right row col =
    let row' = row + down
        col' = col + right
    in if row' >= length grid -- stop once we reach the bottom
       then 0
       else isTree grid row' col' + stepCount grid down right row' col'

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = lines input
        execSlope right down = stepCount grid down right 0 0
    print $ execSlope 3 1
    print . product . map (uncurry execSlope) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]