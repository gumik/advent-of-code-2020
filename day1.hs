import Data.List
import Text.Printf

--------------------------------------------------------------------------------

main = do
    contents <- getContents
    let splitted = lines contents
        numbers = map read splitted :: [Int]
        (num1, num2) = solve numbers
    print numbers
    printf "%d * %d = %d\n" num1 num2 (num1*num2)

--------------------------------------------------------------------------------

solve (number:rest) = if complement `elem` rest
    then (number, complement)
    else solve rest
    where complement = 2020 - number
