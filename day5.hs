import Text.Printf

--------------------------------------------------------------------------------

main = do
    contents <- getContents
    let input = lines contents
    print $ solve input

--------------------------------------------------------------------------------

solve = maximum . map convert where
    convert = sum . map (uncurry (*)) . zip coeffs . reverse . map toBin
    coeffs = iterate (*2) 1
    toBin c = case c of
        'R' -> 1
        'L' -> 0
        'B' -> 1
        'F' -> 0
