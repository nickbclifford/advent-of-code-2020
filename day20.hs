import Control.Applicative
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.ReadP
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

hflip = tf $ map reverse
vflip = tf $ transpose . map reverse . transpose

rotate = tf $ map reverse . transpose

transformations = liftA2 (.) [id, hflip, vflip] [id, rotate, rotate . rotate, rotate . rotate . rotate]

matches t1 t2
    | topEdge t1 == bottomEdge t2 = Just (t2, (0, -1))
    | rightEdge t1 == leftEdge t2 = Just (t2, (1, 0))
    | bottomEdge t1 == topEdge t2 = Just (t2, (0, 1))
    | leftEdge t1 == rightEdge t2 = Just (t2, (-1, 0))
    | otherwise = Nothing

nextTile [] picMap = picMap
nextTile remaining picMap =
    let tfs = transformations <*> remaining
        connections = M.map (\t -> mapMaybe (matches t) tfs) picMap
        Just ((x, y), (tile, (dx, dy)):_) = find (not . null . snd) . M.toList $ connections
    in nextTile (delete tile remaining) (M.insert (x + dx, y + dy) tile picMap)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let All tiles@(t:ts) = read input
        dim = round . sqrt . fromIntegral . length $ tiles
        picTiles = chunkList dim . M.elems $ nextTile ts (M.singleton (0, 0) t)
        corners = map ($ picTiles) [head . head, last . head, head . last, last . last]
    print picTiles
    print . product . map tileId $ corners