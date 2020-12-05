import Data.List
import Utils

binSearch :: Char -> Int -> Int -> String -> Int
binSearch _ _ max [] = max
binSearch highChar min max (c:rest) =
    let mid = (min + max) `div` 2
    in if c == highChar
       then binSearch highChar mid max rest
       else binSearch highChar min mid rest

findId :: [Int] -> Int
findId (i1:rest@(i2:_)) =
    if i2 /= i1 + 1
    then i2 - 1
    else findId rest

main :: IO ()
main = do
    input <- readFile "input.txt"
    let passes = lines input
        rows = map (binSearch 'B' 0 127 . take 7) passes
        cols = map (binSearch 'R' 0 7   . drop 7) passes
        ids = zipWith ((+) . (* 8)) rows cols
    print $ maximum ids
    print . findId . sort $ ids