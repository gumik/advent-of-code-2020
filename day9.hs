import Text.Printf
import Data.List

main = do
    contents <- getContents
    let input = parse contents
        solution1 = solve1 input
    print solution1
    print $ solve2 input solution1

parse :: String -> [Int]
parse = map read . lines

solve1 input = solve1' $ splitAt 25 input where
    solve1' (begin, n:rest) = if isSum begin n
        then solve1' (tail begin ++ [n], rest)
        else n
    isSum l n = case l of
        [_] -> False
        x:xs -> (n - x) `elem` xs || isSum xs n

solve2 input n = solve2' [head input] (tail input) where
    solve2' part1 part2 = if sum part1 < n
        then solve2' (part1 ++ [head part2]) (tail part2)
        else if sum part1 > n
            then if length part1 == 1
                then solve2' [head part2] (tail part2)
                else solve2' (tail part1) part2
            else minimum part1 + maximum part1
