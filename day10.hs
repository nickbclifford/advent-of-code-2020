import Control.Monad.State
import Data.List
import qualified Data.Map.Strict as M
import Utils

countPaths :: [Int] -> Int -> State (M.Map Int Integer) Integer
countPaths adapters jolt = do
    memo <- get
    if jolt == 0
    then return 1
    else case M.lookup jolt memo of
        Just answer -> return answer
        Nothing -> do
            paths <- sequence [countPaths adapters jolt' | jolt' <- takeWhile (< jolt) adapters, jolt - jolt' <= 3]
            let answer = sum paths
            put $ M.insert jolt answer memo
            return answer

main :: IO ()
main = do
    input <- readFile "input.txt"
    let adapters = map read . lines $ input
        builtIn = maximum adapters + 3
        allAdapters = sort $ 0 : builtIn : adapters
        diffs = map (foldr1 subtract) . windows 2 $ allAdapters
    print $ count 1 diffs * count 3 diffs
    print $ evalState (countPaths allAdapters builtIn) M.empty