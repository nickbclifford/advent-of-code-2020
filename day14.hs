import Data.Bits
import Data.List
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP
import Utils

parseMask :: ReadP Instruction
parseMask = Mask
    <$  string "mask = "
    <*> stringSize 36

parseMem :: ReadP Instruction
parseMem = Mem
    <$  string "mem["
    <*> parseInt
    <*  string "] = "
    <*> parseInt

data Instruction = Mask String | Mem Int Int

instance Read Instruction where
    readsPrec _ = readP_to_S (parseMask +++ parseMem)

data MachineState = MState { currentMask :: String, memory :: M.Map Int Int }

runInstruction1 :: MachineState -> Instruction -> MachineState
runInstruction1 s (Mask mStr) = s { currentMask = mStr }
runInstruction1 s@(MState mask mem) (Mem idx val) =
    let bits = zip [0..] (reverse mask)
        processBit (idx, mask) = case mask of
            '0' -> flip clearBit idx
            '1' -> flip setBit idx
            'X' -> id
    in s { memory = M.insert idx (foldr processBit val bits) mem }

runInstruction2 :: MachineState -> Instruction -> MachineState
runInstruction2 s (Mask mStr) = s { currentMask = mStr }
runInstruction2 s@(MState mask mem) (Mem idx val) =
    let bits = zip [0..] (reverse mask)
        buildAddrs (i, mBit) = (<*>) $ case mBit of
            '0' -> [id]
            '1' -> [flip setBit i]
            'X' -> [flip setBit i, flip clearBit i]
    in s { memory = foldr (`M.insert` val) mem (foldr buildAddrs [idx] bits) }

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map read . lines $ input
        initialState = MState { currentMask = "", memory = M.empty }
        -- instructions must be executed in order, so foldl'
        MState _ memory1 = foldl' runInstruction1 initialState instructions
        MState _ memory2 = foldl' runInstruction2 initialState instructions
    print $ sum memory1
    print $ sum memory2