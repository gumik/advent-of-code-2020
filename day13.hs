-- import Data.List.Extra
import Data.List
import Text.Printf

main = do
    --contents <- getContents
    --let input = parse contents
    let input = (939, [7,13,59,31,19])
    print $ solve input
    
--parse str = (timestamp, numbers) where
--    [timestamp, numbersStr] = lines str
--    numbers = map read $ filter (/= "x") $ splitOn "," numbersStr :: [Int]

--solve :: (Int, [Int]) -> [Int]
solve (timestamp, numbers) = (arrival - timestamp) * number where
    (arrival, number) = head $ sort $ map calc numbers 
    calc x = (if n >= timestamp then n else n + x, x) where
        n = x * (timestamp `div` x)