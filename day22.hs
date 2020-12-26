import Control.Arrow
import Data.List.Extra
import Data.Maybe

main = do
    cont <- getContents
    let input = parse cont
    print $ solve input
    print $ solve' input

parse :: String -> ([Int], [Int])
parse input = (pl1, pl2) where
    [pl1, pl2] = parse' input
    parse' = splitOn "\n\n" >>> map (lines >>> tail >>> map read)

solve = Just >>> iterate (turn . fromJust) >>> takeWhile isJust >>> last >>> fromJust >>> winner >>> points


turn :: ([Int], [Int]) -> Maybe ([Int], [Int])
turn ([], _) = Nothing
turn (_, []) = Nothing
turn st@(card1:rest1, card2:rest2) = if card1 > card2
    then Just $ p1win st
    else Just $ p2win st
    
winner ([], x) = x
winner (x, []) = x

points = reverse >>> zipWith (*) [1..] >>> sum

p1win (card1:rest1, card2:rest2) = (rest1 ++ [card1, card2], rest2)
p2win (card1:rest1, card2:rest2) = (rest1, rest2 ++ [card2, card1])

solve' input = points $ deck $ turn' $ Normal input [] 

type Decks = ([Int], [Int])
data GameState = Normal Decks [Decks] | FirstWin [Int] | SecondWin [Int] deriving (Show)
deck (FirstWin d) = d
deck (SecondWin d) = d

turn' :: GameState -> GameState
turn' (Normal st@(d1@(card1:rest1), d2@(card2:rest2)) pastDecks)
    | null d1   =  SecondWin d2
    | null d2 || st `elem` pastDecks  =  FirstWin d1
    | card1 > length rest1 || card2 > length rest2  = turn' $ Normal (fromJust $ turn st) (st:pastDecks)
    | otherwise  =  let subwin = turn' (Normal (take card1 rest1, take card2 rest2) []) in case subwin of
        FirstWin _ -> turn' $ Normal (p1win st) (st:pastDecks)
        SecondWin _ -> turn' $ Normal (p2win st) (st:pastDecks)
turn' (Normal ([], d) _) = SecondWin d
turn' (Normal (d, []) _) = FirstWin d
turn' x = x
