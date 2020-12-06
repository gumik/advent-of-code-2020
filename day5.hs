import Data.List
import Text.Printf

main = do
    contents <- getContents
    let input = lines contents
    print $ solve input
    print $ solve' input

solve = maximum . map convert

solve' input = fst gap + 1 where
    seats = sort $ map convert input
    [gap] = filter (\(s1, s2) -> s2 - s1 > 1) $ zip seats (tail seats)

convert = sum . zipWith (*) coeffs . reverse . map toBin where
    coeffs = iterate (*2) 1
    toBin c = case c of
        'R' -> 1
        'L' -> 0
        'B' -> 1
        'F' -> 0
