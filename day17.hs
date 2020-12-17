{-# LANGUAGE FlexibleInstances #-}

import Data.Either
import qualified Data.Map.Strict as M
import Utils

class Ord a => Index a where
    neighbors :: a -> [a]

instance Index (Int, Int, Int) where
    neighbors (x, y, z) = [(x + dx, y + dy, z + dz) |
        dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], (dx, dy, dz) /= (0, 0, 0)]

instance Index (Int, Int, Int, Int) where
    neighbors (x, y, z, w) = [(x + dx, y + dy, z + dz, w + dw) |
        dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1], (dx, dy, dz, dw) /= (0, 0, 0, 0)]

step :: Index a => M.Map a Char -> M.Map a Char
step dim =
    let findNeighbor idx = case M.lookup idx dim of
            Just c -> Right c
            Nothing -> Left idx
        neighborsAround = partitionEithers . map findNeighbor . neighbors
        next c ns = case c of
            '#' | activeN == 2 || activeN == 3 -> '#'
            '.' | activeN == 3 -> '#'
            _ -> '.'
            where activeN = count '#' ns
        (unknown, nextKnown) = partitionEithers $ do
            (idx, c) <- M.toList dim
            let (empty, ns) = neighborsAround idx
            Right (idx, next c ns) : map Left empty
        nextUnknown = [(idx, if count '#' ns == 3 then '#' else '.') |
            idx <- unknown,
            let (_, ns) = neighborsAround idx]
    in M.fromList nextKnown <> M.fromList nextUnknown

main :: IO ()
main = do
    input <- readFile "input.txt"
    let list = lines input
        lenR = [0..length list - 1]
        init3 = M.fromList [ ((x, y, 0 :: Int)          , list !! y !! x) | x <- lenR, y <- lenR]
        init4 = M.fromList [ ((x, y, 0 :: Int, 0 :: Int), list !! y !! x) | x <- lenR, y <- lenR]
    -- do stuff
    print . count '#' . M.elems $ nTimes step init3 6
    print . count '#' . M.elems $ nTimes step init4 6