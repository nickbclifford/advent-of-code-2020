import Control.Monad.State

type Instruction = (Char, Int)
type Direction = Char
data Position = Pos { x :: Int, y :: Int }

rotateLeft :: Int -> Direction -> Direction
rotateLeft deg dir =
    if deg == 0
    then dir
    else case dir of
        'N' -> next 'W'
        'E' -> next 'N'
        'S' -> next 'E'
        'W' -> next 'S'
        where next = rotateLeft (deg - 90)

rotateRight :: Int -> Direction -> Direction
rotateRight deg dir =
    if deg == 0
    then dir
    else case dir of
        'N' -> next 'E'
        'E' -> next 'S'
        'S' -> next 'W'
        'W' -> next 'N'
        where next = rotateRight (deg - 90)

move :: Instruction -> State (Direction, Position) ()
move (action, param) = do
    (dir, p@(Pos x y)) <- get
    case action of
        'N' -> put (dir, p { y = y + param })
        'E' -> put (dir, p { x = x + param })
        'S' -> put (dir, p { y = y - param })
        'W' -> put (dir, p { x = x - param })
        'L' -> put (rotateLeft  param dir, p)
        'R' -> put (rotateRight param dir, p)
        'F' -> move (dir, param)

rotateWaypoint :: Int -> Position -> Position
rotateWaypoint deg (Pos x y) = Pos
    {
        x = (x * rCos) - (y * rSin),
        y = (x * rSin) + (y * rCos)
    }
    where rad = fromIntegral deg * (pi / 180)
          rSin = round (sin rad)
          rCos = round (cos rad)

moveWaypoint :: Instruction -> State Position (Int, Int)
moveWaypoint (action, param) = do
    p@(Pos x y) <- get
    if action == 'F'
    then return (param * x, param * y)
    else case action of
        'N' -> put $ p { y = y + param }
        'E' -> put $ p { x = x + param }
        'S' -> put $ p { y = y - param }
        'W' -> put $ p { x = x - param }
        'L' -> put $ rotateWaypoint   param  p
        'R' -> put $ rotateWaypoint (-param) p
        >> return (0, 0)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map (\(a:p) -> (a, read p)) (lines input)
        Pos x1 y1 = snd . execState (traverse move instructions) $ ('E', Pos 0 0)
        (x2, y2) = foldr1 (\(ex, ey) (ax, ay) -> (ex + ax, ey + ay)) . evalState (traverse moveWaypoint instructions) $ Pos 10 1
    print $ abs x1 + abs y1
    print $ abs x2 + abs y2