{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Utils

data Square = Open | Tree deriving Eq

fromChar = \case
    '.' -> Open
    '#' -> Tree

type Grid = [[Square]]

idxGrid :: Grid -> Int -> Int -> Square
idxGrid grid row col = 
    if row >= length grid
    then Open
    else rowList !! (col `mod` length rowList)
    where rowList = grid !! row

doSlope :: Grid -> Int -> Int -> State (Int, Int) Square
doSlope grid down right = do
    (row, col) <- get
    let row' = row + down
        col' = col + right
    put (row', col')
    return $ idxGrid grid row' col'

countSlope :: [[Square]] -> Int -> Int -> Int
countSlope grid down right = count Tree . flip evalState (0, 0) . replicateM (length grid - 1) $ doSlope grid down right

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = map (map fromChar) $ lines input
    print $ countSlope grid 1 3
    print . product . map (uncurry $ countSlope grid) $ [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]