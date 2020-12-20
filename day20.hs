import Data.List
import Data.Function
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

matches t1 t2 = or [
        topEdge t1 == bottomEdge t2,
        rightEdge t1 == leftEdge t2,
        bottomEdge t1 == topEdge t2,
        leftEdge t1 == rightEdge t2
    ]

data Tag = Id | HFlip | VFlip | RotateCCW | RotateCW | RotateTwice deriving (Show, Eq)

transformations = [(Id, id), (HFlip, hflip), (VFlip, vflip), (RotateCCW, rotateCcw), (RotateCW, rotateCw), (RotateTwice, rotateCw . rotateCw)]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let All tiles = read input
        constraints = [ ((tag1, tileId t1), (tag2, tileId t2)) |
                t1 <- tiles,
                t2 <- tiles,
                tileId t1 /= tileId t2,
                (tag1, tf1) <- transformations,
                (tag2, tf2) <- transformations,
                (tag1, tag2) == (Id, Id) || tag1 /= tag2,
                matches (tf1 t1) (tf2 t2)
            ]
    print $ length constraints