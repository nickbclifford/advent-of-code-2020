import Data.Array
import Utils

type Index = (Int, Int)
type Seats = Array Index Char
type Size = (Int, Int)

directions :: [(Int, Int)] -- we don't directly index with these so I don't want to call them Indexes
directions = [dir | dx <- [-1..1], dy <- [-1..1], let dir = (dx, dy), dir /= (0, 0)]

occupied :: Size -> Seats -> Index -> Int
occupied (sizeX, sizeY) seats (x, y) = count '#' [getSeat (x + dx) (y + dy) | (dx, dy) <- directions ]
    where getSeat x y =
            if x > sizeX || x < 1 || y > sizeY || y < 1
            then '.'
            else seats ! (x, y)

occupiedInDir :: Size -> Seats -> Index -> Int
occupiedInDir (sizeX, sizeY) seats (x, y) = count '#' [getSeat (x + dx) (y + dy) dx dy | (dx, dy) <- directions]
    where getSeat x' y' dx dy
            | x' > sizeX || x' < 1 || y' > sizeY || y' < 1 = '.'
            | seat /= '.' = seat
            | otherwise = getSeat (x' + dx) (y' + dy) dx dy
            where seat = seats ! (x', y')

type Step = Size -> Seats -> Seats

stepAdj :: Step
stepAdj size seats = array ((1, 1), size) $ do
    (idx, seat) <- assocs seats
    return (idx, case seats ! idx of
            'L' | occupied size seats idx == 0 -> '#'
            '#' | occupied size seats idx >= 4 -> 'L'
            c -> c)

stepDir :: Step
stepDir size seats = array ((1, 1), size) $ do
    (idx, seat) <- assocs seats
    return (idx, case seats ! idx of
            'L' | occupiedInDir size seats idx == 0 -> '#'
            '#' | occupiedInDir size seats idx >= 5 -> 'L'
            c -> c)

untilEq :: Size -> Step -> Seats -> Seats
untilEq size step current =
    let next = step size current in
    if next == current
    then next
    else untilEq size step next

main :: IO ()
main = do
    input <- readFile "input.txt"
    let seatsStrs = lines input
        sizeY = length seatsStrs
        sizeX = length (head seatsStrs)
        size = (sizeX, sizeY)
        seats = array ((1, 1), size) $
            [((x, y), seatsStrs !! (y - 1) !! (x - 1)) | x <- [1..sizeX], y <- [1..sizeY]]
    print . count '#' . elems $ untilEq size stepAdj seats
    print . count '#' . elems $ untilEq size stepDir seats