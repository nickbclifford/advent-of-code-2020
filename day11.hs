import Utils

type Size = (Int, Int)

directions :: [(Int, Int)]
directions = [dir | dx <- [-1..1], dy <- [-1..1], let dir = (dx, dy), dir /= (0, 0)]

occupied :: Size -> [[Char]] -> Int -> Int -> Int
occupied (sizeX, sizeY) seats x y = count '#' [getSeat (x + dx) (y + dy) | (dx, dy) <- directions ]
    where getSeat x y =
            if y >= sizeY || y < 0
            then '.'
            else 
                if x >= sizeX || x < 0
                then '.'
                else seats !! y !! x

occupiedInDir :: Size -> [[Char]] -> Int -> Int -> Int
occupiedInDir (sizeX, sizeY) seats x y = count '#' [getSeat (x + dx) (y + dy) dx dy | (dx, dy) <- directions]
    where getSeat x' y' dx dy
            | x' >= sizeX || x' < 0 || y' >= sizeY || y' < 0 = '.'
            | seat /= '.' = seat
            | otherwise = getSeat (x' + dx) (y' + dy) dx dy
            where seat = (seats !! y' !! x')

stepAdj :: Size -> [[Char]] -> [[Char]]
stepAdj size@(sizeX, sizeY) seats = 
    [ [
        case row !! x of
            'L' | occupied size seats x y == 0 -> '#'
            '#' | occupied size seats x y >= 4 -> 'L'
            c -> c
        | x <- [0..sizeX - 1], let row = seats !! y ]
    | y <- [0..sizeY- 1] ]

stepDir :: Size -> [[Char]] -> [[Char]]
stepDir size@(sizeX, sizeY) seats = 
    [ [
        case row !! x of
            'L' | occupiedInDir size seats x y == 0 -> '#'
            '#' | occupiedInDir size seats x y >= 5 -> 'L'
            c -> c
        | x <- [0..sizeX - 1], let row = seats !! y ]
    | y <- [0..sizeY- 1] ]

untilEq :: Size -> (Size -> [[Char]] -> [[Char]]) -> [[Char]] -> [[Char]] -> [[Char]]
untilEq size step prev seats =
    let seats' = step size seats in
    if prev == seats'
    then seats'
    else untilEq size step seats seats'

main :: IO ()
main = do
    input <- readFile "input.txt"
    let seats = lines input
        sizeY = length seats
        sizeX = length (head seats)
        size = (sizeX, sizeY)
    print . sum . map (count '#') $ untilEq size stepAdj seats seats
    print . sum . map (count '#') $ untilEq size stepDir seats seats