import Data.List
import Data.Maybe
import Utils

findSum :: Int -> [Int] -> Maybe (Int, Int)
findSum n [] = Nothing
findSum n (x:xs) = case find ((== n) . (+ x)) xs of
    Just y -> Just (x, y)
    Nothing -> findSum n xs

find3Sum :: [Int] -> Maybe (Int, Int, Int)
find3Sum [] = Nothing
find3Sum (x:xs) = case findSum (2020 - x) xs of
    Just (y, z) -> Just (x, y, z)
    Nothing -> find3Sum xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let entries = map read $ lines input
        (a, b) = fromJust $ findSum 2020 entries
        (x, y, z) = fromJust $ find3Sum entries
    print $ a * b
    print $ x * y * z