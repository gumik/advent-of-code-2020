import Text.Printf

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    print $ solve' input

parse = map parseLine . lines where
    parseLine line = (cmd, read value :: Int) where
        (cmd:value) = line

solve = getDist . foldl step ((0, 0), 'E') where
    step (pos@(x, y), dir) inst@(cmd,value)
        | cmd `elem` dirs = (move inst pos, dir)
        | cmd == 'F' = (move (dir, value) pos, dir)
        | otherwise = ((x, y), rotate dir cmd value)

move (cmd,value) (x, y) = case cmd of
    'N' -> (x, y - value)
    'S' -> (x, y + value)
    'E' -> (x + value, y)
    'W' -> (x - value, y)

rotate dir cmd value = dropWhile (/= dir) (cycle dirs) !! n where
    n = 4 + m * (value `div` 90)
    m = if cmd == 'R' then 1 else -1

dirs = "NESW"

getDist ((x, y), _) = abs x + abs y

-- solve and solve' may be probably extracted into common code
solve' = getDist . foldl step ((0, 0), (10, -1)) where
    step (pos@(x, y), waypoint) inst@(cmd,value)
        | cmd `elem` dirs = (pos, move inst waypoint)
        | cmd == 'F' = (moveToWaypoint pos waypoint value, waypoint) --
        | otherwise = ((x, y), rotateWaypoint waypoint cmd value)

rotateWaypoint waypoint cmd value = iterate rotation waypoint !! (value `div` 90) where
    rotation = case cmd of
        'R' -> rotateRight
        'L' -> rotateLeft
    rotateRight (x, y) = (-y, x)
    rotateLeft  (x, y) = (y, -x)

moveToWaypoint (x, y) (wx, wy) value = (x + wx * value, y + wy * value)