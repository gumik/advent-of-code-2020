import Text.Printf
import Data.List

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    print $ solve' input

parse :: String -> [Int]
parse = map read . lines

solve input = joltsCount 1 * joltsCount 3  where
    joltsCount n = length $ filter (==n) differences
    differences = zipWith (-) (tail withSourceAndDevice) withSourceAndDevice
    withSourceAndDevice = addSourceAndDevice input

-- (a -> b -> b) -> b -> [a] -> b

solve' input =  head $ foldr step [1] reachCounts where
    step :: Int -> [Int] -> [Int]
    step n prev = sum (take n prev) : prev
    reachCounts = init $ map reachCount (init $ tails withSourceAndDevice)
    reachCount l = case l of
        (x:xs) -> length $ takeWhile ((<= 3) . (+ (-x))) xs
        _ -> 0
    withSourceAndDevice = addSourceAndDevice input


addSourceAndDevice input = 0 : sorted ++ [last sorted + 3] where
    sorted = sort input