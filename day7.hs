import Data.List.Extra
import Text.Printf
import qualified Data.Map as M
import Data.Map ((!), member)

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input

parse = map parseLine . lines where
    parseLine line = (bag, parseContains containsStr) where
        [bag, containsStr] = splitOn " bags contain " line
        parseContains "no other bags." = []
        parseContains str = map parseQuantityStr $ splitOn ", " str
        parseQuantityStr str = (read part1 :: Int, part2 ++ " " ++ part3) where
            part1:part2:part3:_ = splitOn " " str

solve input = length nodesReachedFromShinyGold - 1 where
    nodesReachedFromShinyGold = nub $ concat $ takeWhile (/= []) $ iterate nextNodes ["shiny gold"]
    nextNodes nodes = concatMap (graphMap !) (nodes \\ leaves) where
        leaves = filter (not . (`member` graphMap)) nodes
    graphMap = M.fromListWith (++) $ concatMap rev input
    rev (bag, contain) = map (\(n, x) -> (x, [bag])) contain
