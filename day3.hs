import Data.List
import Data.List.Extra
import qualified Data.Text as T
import Text.Printf

--------------------------------------------------------------------------------

main = do
    contents <- getContents
    let input = lines contents
        slopes = [(1,1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print $ solve input 3 1
    print $ product $ map (uncurry $ solve input) slopes

--------------------------------------------------------------------------------

solve lines stepsRight stepsDown = length $ filter (== '#') points where
    points = zipWith (!!) (map (concat . repeat) lines') [0,stepsRight..]
    lines' = map (lines !!) [0,stepsDown .. length lines - 1]
