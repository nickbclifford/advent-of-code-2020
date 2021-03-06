import qualified Data.Map.Strict as M
import Utils

type State = (Int, Int, M.Map Int Int)

turn :: State -> State
turn (n, last, nums) =
    let val = case M.lookup last nums of
            Just i -> n - i
            Nothing -> 0
    in val `seq` (n + 1, val, M.insert last n nums)

main :: IO ()
main = do
    let input = [17,1,3,16,19,0]
        nums =  M.fromList $ zip (init input) [1..]
        getFinal n = 
            let (_, final, _) = nTimes turn (length input, last input, nums) (n - length input)
            in final
    print $ getFinal 2020
    print $ getFinal 30000000