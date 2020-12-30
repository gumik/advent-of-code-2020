import qualified Data.Map.Strict as M
import qualified Data.Set as S

main = do
    contents <- getContents
    let input = parse contents
        tiles = floorFromPaths input
    print $ solve tiles
    print $ solve' tiles

data Direction = NW | NE | E | SE | SW | W deriving (Show)
type Point = (Int, Int)


parse :: String -> [[Direction]]
parse = map parse' . lines
parse' input = case input of
    [] -> []
    ('n':'w':r) -> NW : parse' r
    ('n':'e':r) -> NE : parse' r
    ('e':r)     ->  E : parse' r
    ('s':'e':r) -> SE : parse' r
    ('s':'w':r) -> SW : parse' r
    (w':r)      ->  W : parse' r

solve :: S.Set Point -> Int
solve = S.size

floorFromPaths :: [[Direction]] -> S.Set Point
floorFromPaths = S.fromList . M.keys . M.filter ((==1).(`mod`2)) .  M.fromListWith (+) . map (flip (,) 1) . map walkPath

walkPath :: [Direction] -> Point
walkPath = last . scanl move (0, 0)

move :: Point -> Direction -> Point
move (x, y) dir = case dir of
    NW -> (x,   y-1)
    NE -> (x+1, y-1)
    E  -> (x+1, y  )
    SE -> (x,   y+1)
    SW -> (x-1, y+1)
    W  -> (x-1, y  )

solve' :: S.Set Point -> Int
solve' = S.size . head . drop 100 . iterate flipTiles

flipTiles :: S.Set Point -> S.Set Point
flipTiles tiles = S.fromList $ filter (isBlack tiles) $ pointsToCheck tiles

pointsToCheck :: S.Set Point -> [Point]
pointsToCheck =  S.toList . S.fromList . concatMap neighboursAndPoint . S.toList

neighbours :: Point -> [Point]
neighbours point = map (move point) [NW, NE, E, SE, SW, W]

neighboursAndPoint :: Point -> [Point]
neighboursAndPoint point = point : neighbours point

isBlack :: S.Set Point -> Point -> Bool
isBlack tiles point = if point `S.member` tiles
    then blackNeighboursCount > 0 && blackNeighboursCount < 3
    else blackNeighboursCount == 2
  where
    blackNeighboursCount = blackNeighbours tiles point

blackNeighbours :: S.Set Point -> Point -> Int
blackNeighbours tiles = length . filter (`S.member` tiles) . neighbours