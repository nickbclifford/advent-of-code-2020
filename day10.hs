import Control.Monad.State
import Data.List
import qualified Data.Map.Strict as M
import Utils

main :: IO ()
main = do
    input <- readFile "input.txt"
    let adapters = map read . lines $ input :: [Int]
        builtIn = maximum adapters + 3
        allAdapters = sort $ 0 : builtIn : adapters
        diffs = map (foldr1 subtract) . windows 2 $ allAdapters
        countPaths :: Int -> State (M.Map Int Integer) Integer
        countPaths jolt =
            if jolt == 0
            then return 1
            else do
                memo <- get
                case M.lookup jolt memo of
                    Just answer -> return answer
                    Nothing -> do
                        paths <- sequence [countPaths jolt' | jolt' <- takeWhile (< jolt) allAdapters, jolt - jolt' <= 3]
                        let answer = sum paths
                        put $ M.insert jolt answer memo
                        return answer
    print $ (count 1 diffs) * (count 3 diffs)
    print $ evalState (countPaths builtIn) M.empty