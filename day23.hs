import Data.List
import Data.Maybe

main = do
    let input = parse "186524973"
    putStrLn $ toString $ solve input

parse :: String -> [Int]
parse = map (read . (:[]))

solve = rotateAfterOne . last . take 101 . iterate step

step (h:x1:x2:x3:xs) = put xs [x1,x2,x3] h ++ [h]

put :: [Int] -> [Int] -> Int -> [Int]
put list items currItem = take n list ++ items ++ drop n list where
    n = (fromJust $ findIndex (==(nextLower list currItem)) list) + 1
    
nextLower :: [Int] -> Int -> Int
nextLower list n = head $ dropWhile (not . flip elem list) (map (flip mod 10) [n-1,n-2..])

rotateAfterOne :: [Int] -> [Int]
rotateAfterOne = tail . uncurry (flip (++)) . break (==1)
--rotateAfterOne list = tail p2 ++ p1 ++ [head p2] where
--    (p1, p2) = break (==1) list

toString :: [Int] -> String
toString = concatMap show