import Data.Char
import Data.Foldable
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP
import Utils

parseBag :: ReadP String
parseBag = (\a b -> a ++ " " ++ b)
    <$> munch1 isLower
    <*  char ' '
    <*> munch1 isLower
    <*  string " bag"
    <*  optional (char 's')

type Amount = (Int, String)
type Rules = M.Map String [Amount]
newtype RuleT = Rule { getRule :: Rules }

parseAmount :: ReadP Amount
parseAmount = (,)
    <$> parseInt
    <*  char ' '
    <*> parseBag

parseRule :: ReadP Rules
parseRule = M.singleton 
    <$> parseBag
    <*  string " contain "
    <*> ([] <$ string "no other bags") <++ (parseAmount `sepBy1` string ", ")
    <*  char '.'

instance Read RuleT where
    readsPrec _ = readP_to_S (Rule <$> parseRule)

containsGold :: Rules -> [String] -> Bool
containsGold rules colors =
    if "shiny gold" `elem` colors
    then True
    else any (containsGold rules . map snd . (rules M.!)) colors

totalBags :: Rules -> String -> Int
totalBags rules color = 1 + sum (map (\(i, c) -> i * totalBags rules c) (rules M.! color))

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rules = mconcat . map (getRule . read) . lines $ input
    print . countPred (containsGold rules . map snd) . toList $ rules
    print $ totalBags rules "shiny gold" - 1 -- totalBags includes the outermost bag, we only want what's inside