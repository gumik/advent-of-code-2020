import Control.Arrow
import Data.List.Extra
import Data.Maybe

main = do
    cont <- getContents
    let input = parse cont
    print $ solve input

parse :: String -> ([Int], [Int])
parse input = (pl1, pl2) where
    [pl1, pl2] = parse' input
    parse' = splitOn "\n\n" >>> map (lines >>> tail >>> map read)

solve = Just >>> iterate (turn . fromJust) >>> takeWhile isJust >>> last >>> fromJust >>> winner >>> reverse >>> zipWith (*) [1..] >>> sum

turn :: ([Int], [Int]) -> Maybe ([Int], [Int])
turn ([], _) = Nothing
turn (_, []) = Nothing
turn (card1:rest1, card2:rest2) = if card1 > card2
    then Just (rest1 ++ [card1, card2], rest2)
    else Just (rest1, rest2 ++ [card2, card1])
    
winner ([], x) = x
winner (x, []) = x
