import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.ReadP hiding (count)
import Utils

data Tile = Tile { tileId :: Int, grid :: [[Char]] }
instance Show Tile where
    show = show . tileId
instance Eq Tile where
    t1 == t2 = tileId t1 == tileId t2

parseTile = Tile
    <$  string "Tile "
    <*> parseInt
    <*  string ":\n"
    <*> stringSize 10 `sepBy1` char '\n'

newtype AllTiles = All [Tile]

parseAllTiles = All <$> parseTile `endBy1` string "\n\n"

instance Read AllTiles where
    readsPrec _ = readP_to_S parseAllTiles

topEdge = head . grid
rightEdge = map last . grid
bottomEdge = last . grid
leftEdge = map head . grid

tf f t = t { grid = f (grid t) }

hflip = map reverse
vflip = transpose . map reverse . transpose
rotate = map reverse . transpose

transformations = liftA2 (.) [id, hflip, vflip] [id, rotate, rotate . rotate, rotate . rotate . rotate]

matches t1 t2
    | topEdge t1 == bottomEdge t2 = Just (t2, (0, -1))
    | rightEdge t1 == leftEdge t2 = Just (t2, (1, 0))
    | bottomEdge t1 == topEdge t2 = Just (t2, (0, 1))
    | leftEdge t1 == rightEdge t2 = Just (t2, (-1, 0))
    | otherwise = Nothing

nextTile [] picMap = picMap
nextTile remaining picMap =
    let tfs = liftA2 tf transformations remaining
        connections = M.map (\t -> mapMaybe (matches t) tfs) picMap
        Just ((x, y), (tile, (dx, dy)):_) = find (not . null . snd) . M.toList $ connections
    in nextTile (delete tile remaining) (M.insert (x + dx, y + dy) tile picMap)

monsters = transformations <*> pure [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
    ]

checkImage :: [[Char]] -> [[Char]] -> Int
checkImage image monster = sum $ do
    cols <- windows (length monster) image
    rows <- map (\c -> windows (length c) c) cols
    let pairs = zipWith zip rows monster
    guard $ countPred (== ('#', '#')) (concat pairs) == 15
    return 15

main :: IO ()
main = do
    input <- readFile "input.txt"
    let All tiles@(t:ts) = read input
        dim = round . sqrt . fromIntegral . length $ tiles
        picTiles = chunkList dim . M.elems $ nextTile ts (M.singleton (0, 0) t)
        corners = map ($ picTiles) [head . head, last . head, head . last, last . last]
        grids = map (map (map (init . tail) . init . tail . grid)) picTiles
        image = concatMap (map concat . transpose) grids
        totalWaves = count '#' (concat image)
        Just monsterWaves = find (/= 0) . map (checkImage image) $ monsters
    print . product . map tileId $ corners
    putStrLn $ unlines image
    print $ totalWaves - monsterWaves