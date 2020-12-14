import Data.List.Extra (splitOn)
import Control.Arrow (first)
import qualified Data.Map as M
import Data.Bits (setBit, clearBit)

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input

type MaskList = [(BitCmd, Int)]
data Command = Mask MaskList | Mem Int Int deriving (Show)
data BitCmd = Set | Unset deriving (Show)

parse = map readLine . lines where
    readLine line = cmd where
        [cmdStr, valueStr] = splitOn " = " line
        cmd = case cmdStr of
            "mask" -> Mask mask
            _      -> Mem (read (init addrStr) :: Int) (read valueStr :: Int)
        [_, addrStr] = splitOn "[" cmdStr
        mask = map (first toBitCmd) $ filter ((/='X') . fst) $ reverse valueStr `zip` [0..]
        toBitCmd c = if c == '1' then Set else Unset

solve = sum . M.elems . fst . foldl step (M.empty :: M.Map Int Int, [] :: MaskList) where
    step (memory, mask) cmd = case cmd of
        Mask mask' -> (memory, mask')
        Mem pos value -> (M.insert pos maskedValue memory, mask) where
            maskedValue = foldl (\v f -> f v) value (map maskBit mask)
            maskBit (cmd, pos) = case cmd of
                Set   -> (`setBit` pos)
                Unset -> (`clearBit` pos)