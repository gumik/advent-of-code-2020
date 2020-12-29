import qualified Data.Map.Strict as M

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input

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

solve = length . filter ((==1) . (`mod` 2)) . M.elems . M.fromListWith (+) . map (flip (,) 1) . map walkPath

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