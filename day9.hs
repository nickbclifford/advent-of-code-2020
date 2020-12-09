import Data.List

-- I think sliding windows are really neat
windows :: Int -> [a] -> [[a]]
windows m xs =
    if length xs == m
    then [xs]
    else (take m xs) : windows m (tail xs)

findInvalidIdx :: [Integer] -> [Integer] -> Int
findInvalidIdx (n:ns) seen =
    if n `elem` [x + y | x <- seen, y <- seen]
    then 1 + findInvalidIdx ns (tail seen ++ [n])
    else 0

findWindow :: Integer -> [Integer] -> [Integer]
findWindow invalid nums = go 1
    where go n = case find ((== invalid) . sum) (windows n nums) of
            Just window -> window
            Nothing -> go (n + 1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let nums = map read . lines $ input
        (preamble, rest) = splitAt 25 nums
        idx = 25 + findInvalidIdx rest preamble
        invalid = nums !! idx
        window = findWindow invalid (take idx nums)
    print invalid
    print $ minimum window + maximum window