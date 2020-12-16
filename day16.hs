import Control.Applicative
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Utils

data Range = Range Int Int

inRange :: Range -> Int -> Bool
inRange (Range lo hi) = liftA2 (&&) (>= lo) (<= hi)

parseRange :: ReadP Range
parseRange = Range
    <$> parseInt
    <*  char '-'
    <*> parseInt

data Field = Field String Range Range

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
        byPos = transpose validTickets
        orderedFields = go byPos fields
            where go [] [] = []
                  go pos remaining = 
                      let Just (fIdx, f) = find (\(_, f) -> countPred (all (validField f)) pos == 1) $ zip [0..] remaining
                          Just pIdx = findIndex (all (validField f)) pos
                          (leftP, _:rightP) = splitAt pIdx pos
                          (leftF, _:rightF) = splitAt fIdx remaining
                          Field name _ _ = f
                          Just ogIdx = (pos !! pIdx) `elemIndex` byPos -- this feels super gross
                      in (ogIdx, name) : go (leftP ++ rightP) (leftF ++ rightF)
    print $ sum invalidFields
    print . product . map ((ticket !!) . fst) . filter (isPrefixOf "departure" . snd) $ orderedFields