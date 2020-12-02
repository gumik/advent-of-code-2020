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
        (num1, num2) = solve numbers
    putStrLn $ show numbers
    printf "%d * %d = %d\n" num1 num2 (num1*num2)

readInt str = number where
    Just (number, _) = B.readInt str

--------------------------------------------------------------------------------

solve (number:rest) = if complement `elem` rest
    then (number, complement)
    else solve rest
    where complement = 2020 - number
