import Data.List.Extra (splitOn)
import Control.Arrow (first)
import qualified Data.Map as M
import Data.Bits (setBit, clearBit)

main = do
    contents <- getContents
    let input = parse contents
    print $ solve step [] input
    print $ solve step' ([], []) input

data Command = Mask String | Mem Int Int deriving (Show)

parse = map readLine . lines where
    readLine line = cmd where
        [cmdStr, valueStr] = splitOn " = " line
        cmd = case cmdStr of
            "mask" -> Mask valueStr
            _      -> Mem (read (init addrStr) :: Int) (read valueStr :: Int)
        [_, addrStr] = splitOn "[" cmdStr


solve step emptyMask = sum . M.elems . fst . foldl step (M.empty, emptyMask)

step (memory, mask) cmd = case cmd of
        Mask mask' -> (memory, map toBitCmd $ filter ((/='X') . fst) $ reverse mask' `zip` [0..])
        Mem pos value -> (M.insert pos maskedValue memory, mask) where
            maskedValue = foldr ($) value mask

toBitCmd (c, pos) = if c == '1' then (`setBit` pos) else (`clearBit` pos)

step' (memory, mask@(ones, floating)) cmd = case cmd of
    Mask mask' -> (memory, parseMask mask')
    Mem pos value -> (foldl (\memory' pos' -> M.insert pos' value memory') memory positions, mask) where
        positions = generatePositions floating maskedPos
        maskedPos = foldr ($) pos ones

parseMask :: String -> ([Int -> Int], [Int])
parseMask mask = (ones, floating) where
    ones = map (\(_, pos) -> (`setBit` pos)) $ filter ((=='1') . fst) x
    floating = map snd $ filter ((=='X') . fst) x
    x = filter ((/='0') . fst) $ reverse mask `zip` [0..]

generatePositions :: [Int] -> Int -> [Int]
generatePositions [] _ = []
generatePositions (pos:rest) value = v1:v2:(generatePositions rest v1 ++ generatePositions rest v2) where
    v1 = value `setBit` pos
    v2 = value `clearBit` pos
