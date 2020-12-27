import Control.Monad ((>>=))
import Data.Array (array, (!))
import Data.Char (isLetter)
import Data.List (break, inits, nub)
import Data.List.Extra (splitOn)
import Data.Maybe (Maybe, fromJust, isJust, mapMaybe)

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input

data Rule = Seq [Int] | Choice Rule Rule | Letter Char | Cycle [Int] [Int] deriving (Show)

parse contents =  (parseRules rules, lines expressions)  where
    [rules, expressions] = splitOn "\n\n" contents

parseRules str = array (0, length l - 1) l where
    l = map parseRule $ lines str

parseRule str = (read n :: Int, rule) where
    [n, ruleStr] = splitOn ":" str
    rule
        | '"' `elem` ruleStr  =  Letter $ head $ filter isLetter ruleStr
        | '|' `elem` ruleStr  =  let (left, right) = break (=="|") $ words ruleStr in Choice (Seq $ map read left) (Seq $ map read $ tail right)
        | '*' `elem` ruleStr  =  let (left, right) = break (=="*") $ words ruleStr in Cycle (map read left) (map read $ filter (/="*") right)
        | otherwise           =  Seq $ map read $ words ruleStr

solve (rules, expressions) = length $ filter (elem "") $ filter (not . null) $ map (evalExpression rules) expressions
evalExpression rules expression = eval (rules ! 0) [expression] where
    eval :: Rule -> [String] -> [String]
    eval _ [] = []
    eval rule strs = case rule of
        Letter c     -> mapMaybe (evalLetter c) strs
        Seq s        -> nub $ concatMap (evalSeq s) strs
        Choice r1 r2 -> nub $ concatMap (evalMany [r1, r2]) strs
        Cycle l r -> nub $ concatMap (evalCycle l r) strs
    evalLetter c str = case str of
        [] -> Nothing
        c':rest -> if c' == c then Just rest else Nothing
    evalSeq s str = foldl (\strs r -> eval (rules ! r) strs) [str] s
    evalMany :: [Rule] -> String -> [String]
    evalMany rs str = nub $ concatMap (\r -> eval r [str]) rs
    evalCycle l r str = evalMany (take cycleDepth $ map Seq $ rules l r) str where
        rules l' r' = (l' ++ r') : rules (extend l') (extend r')

extend x = case x of
    [] -> []
    _  -> head x : x

cycleDepth = 5
