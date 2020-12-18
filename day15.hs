import Data.List.Extra (splitOn)
import Data.Map (Map, fromList, insert, findWithDefault)

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input 2020
    print $ solve input 30000000

parse :: String -> [Int]
parse = map read . splitOn ","


-- solve input n = num where
--     (_, _, num) = steps input !! (n - length input)

-- steps input = iterate step (firstStep input)


solve input n = third $ solve' (firstStep input) (n - length input)
solve' s n
    | n > 0  = s `seq` solve' (step s) (n-1)
    | otherwise  =  s

firstStep input = (fromList (init input `zip` [1..]), length input, last input)

step (prevNums, pos, num) = prevNums `seq` (insert num pos prevNums, pos+1, newNum) where
    newNum = pos - findWithDefault pos num prevNums

third (_, _, x) = x
