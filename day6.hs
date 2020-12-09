import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Set as S

main :: IO ()
main = do
    input <- TI.readFile "input.txt"
    let textGroups = T.splitOn (T.pack "\n\n") input
    print . sum . map (length . S.fromList . filter (/= '\n') . T.unpack) $ textGroups
    print . sum . map (length . foldr1 S.intersection . map S.fromList . lines . T.unpack) $ textGroups