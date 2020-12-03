import Data.List
import Text.Printf

--------------------------------------------------------------------------------

main = do
    contents <- getContents
    let splitted = lines contents
        numbers = map read splitted
        [num1, num2] = solve 2 numbers
        [num1', num2', num3'] = solve 3 numbers
    print numbers
    printf "%d * %d = %d\n" num1 num2 (num1*num2)
    printf "%d * %d * %d = %d\n" num1' num2' num3' (num1'*num2'*num3')

--------------------------------------------------------------------------------

solve :: Int -> [Int] -> [Int]
solve n numbers = head solutions
    where solutions = filter ((== 2020) . sum) combinations
          combinations = comb numbers n

comb :: [Int] -> Int -> [[Int]]
comb (x:xs) 1 = [x]:comb xs 1
comb (x:xs) n = map (x:) (comb xs (n-1)) ++ comb xs n
comb [] _ = []

