import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import Text.Printf
import qualified Data.ByteString.Char8 as B

main = do
    args <- getArgs
    h <- openFile (head args) ReadMode
    contents <- B.hGetContents h
    let splitted = filter (not . B.null) $ B.split '\n' contents
        numbers = map readInt splitted :: [Int]
        [num1, num2] = solve 2 numbers
        [num1', num2', num3'] = solve 3 numbers
    putStrLn $ show numbers
    printf "%d * %d = %d\n" num1 num2 (num1*num2)
    printf "%d * %d * %d = %d\n" num1' num2' num3' (num1'*num2'*num3')

readInt str = number where
    Just (number, _) = B.readInt str

--------------------------------------------------------------------------------

solve :: Int -> [Int] -> [Int]
solve n numbers = head solutions
    where solutions = filter ((== 2020) . sum) combinations
          combinations = comb numbers n

comb :: [Int] -> Int -> [[Int]]
comb (x:xs) 1 = [x]:comb xs 1
comb (x:xs) n = map (x:) (comb xs (n-1)) ++ comb xs n
comb [] _ = []
