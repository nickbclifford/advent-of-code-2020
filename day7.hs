import Control.Applicative (liftA2)
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

newtype Rule = Rule { getRule :: M.Map String [(Int, String)] }

parseRule :: ReadP Rule
parseRule = Rule <$> (M.singleton 
    <$> parseBag
    <*  string " contain "
    <*> (([] <$ string "no other bags") <++ ((,)
        <$> parseInt
        <*  char ' '
        <*> parseBag) `sepBy1` string ", ")
    <*  char '.')

instance Read Rule where
    readsPrec _ = readP_to_S parseRule

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rules = mconcat . map (getRule . read) . lines $ input
        containsGold vals =
            if "shiny gold" `elem` map snd vals
            then True
            else any (\(_, c) -> containsGold (rules M.! c)) vals
        totalBags color = 1 + case rules M.! color of
            [] -> 0
            rules -> sum $ map (\(i, c) -> i * totalBags c) rules
    print . countPred containsGold . toList $ rules
    print $ totalBags "shiny gold" - 1