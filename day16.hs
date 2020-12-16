import Control.Applicative
import Data.Char
import Data.Function
import Data.List
import Text.ParserCombinators.ReadP
import Utils

data Field = Field { name :: String, r1 :: Range, r2 :: Range }

parseField :: ReadP Field
parseField = Field
    <$> munch1 (liftA2 (||) isSpace isAlpha)
    <*  string ": "
    <*> parseRange
    <*  string " or "
    <*> parseRange

validField :: Field -> Int -> Bool
validField (Field _ r1 r2) = liftA2 (||) (inRange r1) (inRange r2)

parseTicket :: ReadP [Int]
parseTicket = parseInt `sepBy1` char ','

data Notes = Notes [Field] [Int] [[Int]]

parseNotes :: ReadP Notes
parseNotes = Notes
    <$> parseField `sepBy1` char '\n'
    <*  string "\n\nyour ticket:\n"
    <*> parseTicket
    <*  string "\n\nnearby tickets:\n"
    <*> parseTicket `sepBy1` char '\n'

instance Read Notes where
    readsPrec _ = readP_to_S parseNotes

main :: IO ()
main = do
    input <- readFile "input.txt"
    let Notes fields ticket nearby = read input
        invalidFields = filter (\i -> not $ any (`validField` i) fields) (concat nearby)
        validTickets = filter (all (\i -> any (`validField` i) fields)) nearby
        fullPos = transpose validTickets
        orderedFields = go fullPos fields
            where go [] [] = []
                  go allPos remaining =
                      let Just f = find (\f -> countPred (all (validField f)) allPos == 1) remaining
                          Just pos = find (all (validField f)) allPos
                          Just ogIdx = pos `elemIndex` fullPos
                      in (ogIdx, name f) : go (delete pos allPos) (deleteBy ((==) `on` name) f remaining)
    print $ sum invalidFields
    print . product . map ((ticket !!) . fst) . filter (isPrefixOf "departure" . snd) $ orderedFields