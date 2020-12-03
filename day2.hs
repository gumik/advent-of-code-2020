import Data.List
import Data.List.Extra
import qualified Data.Text as T
import Text.Printf

--------------------------------------------------------------------------------

main = do
    contents <- getContents
    let content_lines = lines contents
        input = map splitLine content_lines
        occurences = solve input
        occurences' = solve' input
    print occurences
    print occurences'

splitLine line = (rangeMin, rangeMax, letter, pass) where
    [rangeMin, rangeMax] = map read . split (== '-') $ rangeStr :: [Int]
    letter = head letterStr
    [rangeStr, letterStr, pass] = words line

--------------------------------------------------------------------------------

solve :: [(Int, Int, Char, String)] -> Int
solve = length . filter isCorrectPass where
    isCorrectPass (rangeMin, rangeMax, letter, pass) = occurences >= rangeMin && occurences <= rangeMax where
        occurences = length $ filter (== letter) pass

solve' :: [(Int, Int, Char, String)] -> Int
solve' = length . filter isCorrectPass where
    isCorrectPass (index1, index2, letter, pass) = cond1 /= cond2 where
        cond1 = (pass !! (index1 - 1)) == letter
        cond2 = (pass !! (index2 - 1)) == letter
