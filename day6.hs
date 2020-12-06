import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Set as S
import Utils

commonEls :: [S.Set Char] -> S.Set Char
commonEls (x:xs) = 
    if null xs
    then x -- the common elements between a single set is just that set
    else foldr S.intersection x xs

main :: IO ()
main = do
    input <- TI.readFile "input.txt"
    let textGroups = T.splitOn (T.pack "\n\n") input
    print . sum . map (length . S.fromList . filter (/= '\n') . T.unpack) $ textGroups
    print . sum . map (length . commonEls . map S.fromList . lines . T.unpack) $ textGroups