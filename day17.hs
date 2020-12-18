import Data.Map (fromListWith, toList)
import Data.List (sort)

main = do
    cont <- getContents
    let input = parse cont
    print $ solve input
    print $ solve' input

parse = concat . zipWith parseLine [0..] . lines
parseLine y line = map ((,,) 0 y . fst) $ filter ((=='#') . snd) $ [0..] `zip` line

solve input = length $ iterate step input !! 6
step input = map fst $ filter isActive neighboursCounts where
    neighboursCounts = toList $ fromListWith (+) (neighbours `zip` [1,1..])
    neighbours = concatMap getNeighbours input
    isActive (point, count) = (point `elem` input && count `elem` [2,3])
                            || (point `notElem` input && count == 3)
getNeighbours (x, y, z) = [(x', y', z') | x' <- [x-1..x+1]
                                        , y' <- [y-1..y+1]
                                        , z' <- [z-1..z+1]
                                        , (x', y', z') /= (x, y, z)]

-- This may be done as generalized function for n-dimensional space
solve' input = length $ iterate step' (map (\(x, y, z) -> (x, y, z, 0)) input) !! 6
step' input = map fst $ filter isActive neighboursCounts where
    neighboursCounts = toList $ fromListWith (+) (neighbours `zip` [1,1..])
    neighbours = concatMap getNeighbours' input
    isActive (point, count) = (point `elem` input && count `elem` [2,3])
                            || (point `notElem` input && count == 3)
getNeighbours' (x, y, z, w) = [(x', y', z', w') | x' <- [x-1..x+1]
                                                , y' <- [y-1..y+1]
                                                , z' <- [z-1..z+1]
                                                , w' <- [w-1..w+1]
                                                , (x', y', z', w') /= (x, y, z, w)]
