import Data.List.Extra
import Data.List
import Text.Printf

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    print $ solve' input

parse = map words . splitOn "\n\n"

uniqueAnswers = nub . concat

solve = sum . map (length . uniqueAnswers)
solve' = sum . map countForGroup where
    countForGroup group = length $ filter id $ map (\x -> all (x `elem`) group) $ uniqueAnswers group
