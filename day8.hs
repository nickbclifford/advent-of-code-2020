import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
import Utils

data Instruction = Instruction String Int

parseInstruction :: ReadP Instruction
parseInstruction = do
    ins <- stringSize 3
    char ' '
    sign <- char '+' +++ char '-'
    i <- parseInt
    let param = if sign == '-' then negate i else i
    return $ Instruction ins param

instance Read Instruction where
    readsPrec _ = readP_to_S parseInstruction

data Behavior = Loops | Terminates deriving Eq

runProgram :: [Instruction] -> (Int, Int, S.Set Int) -> (Int, Behavior)
runProgram instructions (acc, pc, seen) =
    if pc >= length instructions
    then (acc, Terminates)
    else
        if pc `S.member` seen
        then (acc, Loops)
        else case instructions !! pc of
            Instruction "nop" _ -> runProgram instructions (acc, pc + 1, seen')
            Instruction "acc" p -> runProgram instructions (acc + p, pc + 1, seen')
            Instruction "jmp" p -> runProgram instructions (acc, pc + p, seen')
            where seen' = S.insert pc seen

mutate :: Instruction -> Instruction
mutate i@(Instruction "acc" _) = i
mutate   (Instruction "jmp" p) = Instruction "nop" p
mutate   (Instruction "nop" p) = Instruction "jmp" p

mutateAt :: Int -> [Instruction] -> [Instruction]
mutateAt i xs = case splitAt i xs of
    (first, ins:rest) -> first ++ (mutate ins):rest
    (first, []) -> first

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map read . lines $ input :: [Instruction]
        initState = (0, 0, S.empty)
        mutated = map (\i -> runProgram (mutateAt i instructions) initState) [0..length instructions - 1]
    print . fst $ runProgram instructions initState
    print . fst . fromJust . find ((== Terminates) . snd) $ mutated
