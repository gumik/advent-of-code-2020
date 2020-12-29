import Data.List
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

main = do
    let input = parse "186524973"
    putStrLn $ toString $ solve input
    print $ solve' input

parse :: String -> [Int]
parse = map (read . (:[]))

solve = rotateAfterOne . last . take 101 . iterate step

step :: [Int] -> [Int]
step (h:x1:x2:x3:xs) = put xs [x1,x2,x3] h ++ [h]

put :: [Int] -> [Int] -> Int -> [Int]
put list items currItem = take n list ++ items ++ drop n list where
    n = (fromJust $ findIndex (==(nextLower list currItem)) list) + 1
    
nextLower :: [Int] -> Int -> Int
nextLower list n = head $ dropWhile (not . flip elem list) (map (flip mod 10) [n-1,n-2..])

rotateAfterOne :: [Int] -> [Int]
rotateAfterOne = tail . uncurry (flip (++)) . break (==1)

toString :: [Int] -> String
toString = concatMap show


solve' input = mapToList 0 $ fst $ last $ take 101 $ iterate (step' (maximum input)) (cupsMap input, head input - 1)

cupsMap :: [Int] -> IntMap Int
cupsMap input = M.fromList $ zip input' (tail input' ++ [head input']) where input' = map (+ (-1)) input

--step' :: Int -> (IntMap Int, Int) -> (IntMap Int, Int)
step' maxNum (cups, current) = (adjustedCups, next) where
    adjustedCups = M.adjust (const next) current $ M.adjust (const (picked !! 0)) putPosition $  M.adjust (const afterPutPosition) (picked !! 2) cups
    next4 = take 4 $ tail $ nextCups cups current
    picked = take 3 next4
    next = last next4
    putPosition = nextLower' maxNum picked current
    afterPutPosition = head $ tail $ nextCups cups putPosition
    
nextLower' :: Int -> [Int] -> Int -> Int
nextLower' maxNum justPicked n = head $ dropWhile (`elem` justPicked) (map (`mod` maxNum) [n-1,n-2..])

nextCups :: IntMap Int -> Int -> [Int]
nextCups cups = iterate (cups M.!)

mapToList :: Int -> IntMap Int -> [Int]
mapToList n cups = map(+1) $ take (M.size cups) $ nextCups cups n