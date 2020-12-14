import Control.Arrow (second)
import Data.List.Extra (splitOn)
import Data.List

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    print $ solve' $ snd input

parse str = (read timestampStr :: Int, numbers) where
    [timestampStr, numbersStr] = lines str
    numbers = map (second read) $ filter ((/= "x") . snd) $ zip [0..] (splitOn "," numbersStr) :: [(Int, Int)]

solve (timestamp, numbers) = (arrival - timestamp) * number where
    (arrival, number) = minimum $ map (calc . snd) numbers
    calc x = (if n >= timestamp then n else n + x, x) where
        n = x * (timestamp `div` x)

solve' = fst . foldl1 step

greatestMultipleLowerThan mult n = (n `div` mult) * mult
lowestMultipleGreaterEqualThan mult n = if m < n then m + mult else m where
    m = greatestMultipleLowerThan mult n


step (start, mult) (pos, num) = (newStart, lcm num mult) where
    (newStart, _) = head $ dropWhile (\(a, b) -> b - a /= pos) $ baseTimestamps `zip` numTimestamps
    baseTimestamps = map (timestamp start mult) [0..]
    numTimestamps = map ((+ greatestMultipleLowerThan num pos) . lowestMultipleGreaterEqualThan num) baseTimestamps

timestamp start mult i = start + (mult * i)
