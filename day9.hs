import Data.List
import Utils

-- stole this from stack overflow lol
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

untilInvalidIdx (n:ns) seen =
    if n `elem` [x + y | x <- seen, y <- seen]
    then 1 + untilInvalidIdx ns (tail seen ++ [n])
    else 0

findWindow invalid nums n = case find ((== invalid) . sum) (windows n nums) of
    Just window -> window
    Nothing -> findWindow invalid nums (n + 1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let nums = map read . lines $ input :: [Integer]
        (preamble, rest) = splitAt 25 nums
        idx = 25 + untilInvalidIdx rest preamble
        invalid = nums !! idx
        window = findWindow invalid (take idx nums) 1
    print invalid
    print window
    print $ minimum window + maximum window