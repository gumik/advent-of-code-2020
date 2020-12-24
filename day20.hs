--import Data.List.Split (splitOn)
import Control.Arrow ((&&&), first)
import Data.Map (fromListWith, (!))
import Data.List (transpose, unfoldr, tails)
import Data.Maybe
import qualified Data.Map as M

main = do
    cont <- getContents
    let input = parse cont
    print $ solve input
    print $ solve' input
    
    
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


solve' input = monsters image where
    image = reconstructImage input
    
reconstructImage input = concat $ map (transpose . concat . map (init . tail . transpose . init . tail . snd)) mergedTiles where
    mergedTiles = map (fillRow tiles edgesDct) $ firstColumn
    firstColumn = fillColumn tiles edgesDct cornerPositioned 
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
fillRow = fillLine right left
fillColumn = fillLine bottom top
fillLine side1 side2 tiles edgesDct idAndTile = map fromJust $ takeWhile isJust $ iterate findNext $ Just idAndTile where
    findNext maybeIdAndTile = do
        idAndTile <- maybeIdAndTile
        nextIdAndTile <- findNeighbour tiles edgesDct side1 side2 idAndTile
        return nextIdAndTile
findNeighbour tiles edgesDct side1 side2 (n, tile) = if null validIds then Nothing else Just (neighbourId, neighbourPositioned) where
    neighbourPositioned = head $ filter (\rotation -> side2 rotation == side1 tile) (rotations neighbour)
    neighbour = tiles ! neighbourId
    neighbourId = head $ validIds
    validIds = filter (/= n) $ edgesDct ! side1 tile

rotations tile = (take 4 $ iterate rotate tile) ++ (take 4 $ iterate rotate $ reverse tile)
rotate = reverse . transpose
right = map last
left = map head
top = head
bottom = last

monsters = (\(x, y, z) -> (y, z)) . maximum . map (\img -> (length $ concat $ matches img, matches img, img)) . rotations

matches image =  map matchesOnLines $ tails image
  
matchesOnLines = map fst . filter ((== length monster) . snd) . M.toList . fromListWith (+) . (`zip` [1,1..]) . concat . zipWith matchesOnLine monster
    
matchesOnLine substr = map fst . filter snd . zip [0..] . map (matchBegin $ substr) . tails

matchBegin [] _ = True
matchBegin _ [] = False
matchBegin (m:ms) (c:rest)
    | m == ' '  =  matchBegin ms rest
    | c == m    =  matchBegin ms rest
    | otherwise =  False

monster = ["                  # "
          ,"#    ##    ##    ###"
          ," #  #  #  #  #  #   "]

printTile :: [String] -> IO ()
printTile tile = mapM_ putStrLn tile >> putStrLn ""

-----------

splitOn x str = splitOn' str "" where
    splitOn' str acc
        | length str < l   =  [reverse acc]
        | take l str == x  =  reverse acc : splitOn' (drop l str) ""
        | otherwise        =  splitOn' (drop 1 str) (head str : acc)
    l = length x
    