import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
import Utils

data Instruction = Instruction String Int

parseInstruction = do
    ins <- stringSize 3
    char ' '
    sign <- char '+' +++ char '-'
    i <- parseInt
    let param = if sign == '-' then negate i else i
    return $ Instruction ins param

instance Read Instruction where
    readsPrec _ = readP_to_S parseInstruction

runUntilDup instructions (acc, pc, seen) =
    if pc `S.member` seen
    then acc
    else case instructions !! pc of
        Instruction "nop" _ -> runUntilDup instructions (acc, pc + 1, seen')
        Instruction "acc" p -> runUntilDup instructions (acc + p, pc + 1, seen')
        Instruction "jmp" p -> runUntilDup instructions (acc, pc + p, seen')
        where seen' = S.insert pc seen

doesTerminate instructions (acc, pc, seen) =
    if pc >= length instructions
    then Just acc
    else
        if pc `S.member` seen
        then Nothing
        else case instructions !! pc of
            Instruction "nop" _ -> doesTerminate instructions (acc, pc + 1, seen')
            Instruction "acc" p -> doesTerminate instructions (acc + p, pc + 1, seen')
            Instruction "jmp" p -> doesTerminate instructions (acc, pc + p, seen')
            where seen' = S.insert pc seen

mutate i@(Instruction "acc" _) = i
mutate   (Instruction "jmp" p) = Instruction "nop" p
mutate   (Instruction "nop" p) = Instruction "jmp" p

mutateAt i xs = case splitAt i xs of
    (first, ins:rest) -> first ++ (mutate ins):rest
    (first, []) -> first

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map read . lines $ input :: [Instruction]
    print $ runUntilDup instructions (0, 0, S.empty)
    print . catMaybes $ map (\i -> doesTerminate (mutateAt i instructions) (0, 0, S.empty)) [0..length instructions - 1]
