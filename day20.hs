--import Data.List.Split (splitOn)
import Data.Map (fromListWith)
import Data.List (transpose)
import qualified Data.Map as M

main = do
    cont <- getContents
    let input = parse cont
    print $ solve input
    
parse = map parseTile . splitOn "\n\n"
parseTile str = (num, tile) where
    num = read$ init $ drop 5 header :: Int
    (header:tile) = lines str

solve input = product $ map fst $ filter ((==4).snd) $ M.toList$ M.fromListWith (+) $ (`zip` [1,1..]) $ concat $ M.elems $ M.filter ((==1) . length) $ makeDict input

makeDict = fromListWith (++) . concat . map (\(n, tile) -> edges tile `zip` repeat [n])

edges tile = regular ++ flipped where
    flipped = map reverse regular
    regular = [top, bottom, left, right]
    top = head tile
    bottom = last tile
    left = head transposed
    right = last transposed
    transposed = transpose tile


splitOn x str = splitOn' str "" where
    splitOn' str acc
        | length str < l   =  [reverse acc]
        | take l str == x  =  reverse acc : splitOn' (drop l str) ""
        | otherwise        =  splitOn' (drop 1 str) (head str : acc)
    l = length x