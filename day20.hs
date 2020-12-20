import Data.List
import Text.ParserCombinators.ReadP hiding (count)
import Utils

data Tile = Tile { tileId :: Int, grid :: [[Char]] }

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

rotateCw  = tf $ map reverse . transpose
rotateCcw = tf $ transpose . map reverse

transformations = [id, hflip, vflip, rotateCcw, rotateCw, rotateCw . rotateCw]
getEdges = [topEdge, rightEdge, bottomEdge, leftEdge]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let All tiles = read input
        findConnections t1 = [ tileId t2 |
            t2 <- tiles,
            tileId t1 /= tileId t2,
            let tfs = transformations <*> pure t2,
            let edges = getEdges <*> pure t1,
            any (`elem` edges) (getEdges <*> tfs) ]
        connections = map (\t1 -> (tileId t1, findConnections t1)) tiles
        corners = filter ((== 2) . length . snd) connections
        edges = filter ((== 3) . length . snd) connections
    print . product . map fst $ corners
    print corners
    print edges