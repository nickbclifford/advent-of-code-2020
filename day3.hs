{-# LANGUAGE LambdaCase #-}

import Control.Monad.Reader
import Utils

data Square = Open | Tree deriving Eq

fromChar :: Char -> Square
fromChar = \case
    '.' -> Open
    '#' -> Tree

isTree :: Square -> Integer
isTree = \case
    Open -> 0
    Tree -> 1

type Grid = [[Square]]
type WithGrid = Reader Grid

idxGrid :: Int -> Int -> WithGrid Square
idxGrid row col = do
    grid <- ask
    let rowList = grid !! row
    return $ rowList !! (col `mod` length rowList) -- mod to handle the "repeating"

stepCount :: Int -> Int -> Int -> Int -> WithGrid Integer
stepCount down right row col = do
    grid <- ask
    let row' = row + down
        col' = col + right
    square <- idxGrid row' col'
    total <- stepCount down right row' col'
    return $
        if row' >= length grid -- stop once we reach the bottom
        then 0
        else isTree square + total

countSlope :: Int -> Int -> WithGrid Integer
countSlope right down = stepCount down right 0 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = map (map fromChar) $ lines input
        runPrint = print . flip runReader grid
    runPrint $ countSlope 3 1
    runPrint . fmap product . mapM (uncurry countSlope) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]