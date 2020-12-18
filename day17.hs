
main = do
    cont <- getContents
    let input = parse cont
    print $ solve input
    
parse = concat . map parseLine . zip [0..] . lines
parseLine (y, line) = map ((,) y . fst) $ filter ((=='#') . snd) $ [0..] `zip` line

solve :: [(Int, Int)] -> [(Int, Int)]
solve input = step input
step input = neighbours where
    neighbours = concat $ map getNeighbours input
getNeighbours (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]