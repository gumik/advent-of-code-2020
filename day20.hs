--import Data.List.Split (splitOn)
import Data.Map (fromListWith, (!))
import Data.List (transpose, unfoldr)
import Data.Maybe
import qualified Data.Map as M

main = do
    cont <- getContents
    let input = parse cont
    print $ solve input
    mapM_ (printTile . snd) $ solve' input
    
    
parse = map parseTile . splitOn "\n\n"
parseTile str = (num, tile) where
    num = read$ init $ drop 5 header :: Int
    (header:tile) = lines str


solve input = product $ map fst $ filter ((==4).snd) $ border $ makeDict input

border = M.toList . M.fromListWith (+) . (`zip` [1,1..]) . concat . M.elems . M.filter ((==1) . length)
makeDict = fromListWith (++) . concat . map (\(n, tile) -> edges tile `zip` repeat [n])

edges tile = regular ++ flipped where
    flipped = map reverse regular
    regular = [top tile, bottom tile, left tile, right tile]


solve' input = fillRow tiles edgesDct cornerPositioned where
    cornerPositioned = findFirstCorner edgesDct tiles
    edgesDct = makeDict input
    tiles = M.fromList input

findFirstCorner edgesDct tiles = (tileId, tile) where
    tile = rotateFirstCorner edgesDct $ tiles ! tileId
    tileId = fst corner
    corner = head $ filter ((==4).snd) borders
    borders = border edgesDct

rotateFirstCorner edgesDct tile = head $ filter isCorrect $ iterate rotate tile where
    isCorrect tile = (length (edgesDct ! left tile) == 1)
                  && (length (edgesDct ! top tile) == 1)

fillRow :: M.Map Int [String] -> M.Map String [Int] -> (Int, [String]) -> [(Int, [String])]
fillRow tiles edgesDct idAndTile = unfoldr (\idAndTile -> (findNeighbour tiles edgesDct right left idAndTile) >>= \nextIdAndTile -> return (idAndTile, nextIdAndTile)) idAndTile

findNeighbour tiles edgesDct side1 side2 (n, tile) = if null validIds then Nothing else Just (neighbourId, neighbourPositioned) where
    neighbourPositioned = head $ filter (\rotation -> side2 rotation == side1 tile) rotations
    rotations = (take 4 $ iterate rotate neighbour) ++ (take 4 $ iterate rotate $ reverse neighbour)
    neighbour = tiles ! neighbourId
    neighbourId = head $ validIds
    validIds = filter (/= n) $ edgesDct ! side1 tile

rotate = reverse . transpose
right = map last
left = map head
top = head
bottom = last

printTile :: [String] -> IO ()
printTile tile = mapM_ putStrLn tile >> putStrLn ""

-----------

splitOn x str = splitOn' str "" where
    splitOn' str acc
        | length str < l   =  [reverse acc]
        | take l str == x  =  reverse acc : splitOn' (drop l str) ""
        | otherwise        =  splitOn' (drop 1 str) (head str : acc)
    l = length x