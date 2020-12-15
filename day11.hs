import Data.Array
import Text.Printf

main = do
  contents <- getContents
  let input = parse contents
  print $ solve change input
  print $ solve change' input

data Place = FreeSeat | TakenSeat | Floor | Boundary deriving(Show, Eq)
readPlace c = case c of
  'L' -> FreeSeat
  '#' -> TakenSeat
  '.' -> Floor

parse :: String -> Array (Int, Int) Place
parse input = toArray ([((y, x), Boundary) | x <- [0..cols+1], y <- [0..rows+1]] ++ enumeratedList) where
  toArray = array ((0, 0), (rows+1, cols+1))
  enumeratedList = assocs $ listArray ((1, 1), (rows, cols)) $ concat lists
  lists = map (map readPlace) $ lines input
  rows = length lists
  cols = length $ head lists

solve change arr = length $ filter (== TakenSeat) $ elems $ steps arr where
    steps arr = if newArr == arr
              then arr
              else steps newArr
      where
        newArr = step arr
    (rows, cols) = snd $ bounds arr
    step arr = arr // [((y, x), change arr y x) | y <- [1..rows-1], x <- [1..cols-1]]

-- These two may have a lot of common code.
change arr y x = case place of
    TakenSeat -> if occupied >= 4 then FreeSeat else TakenSeat
    FreeSeat -> if occupied == 0 then TakenSeat else FreeSeat
    Floor -> Floor
  where
    place = arr ! (y, x)
    occupied = length $ filter (== TakenSeat) $ [arr ! (y', x') | y' <- [y-1 .. y+1], x' <- [x-1 .. x+1], (y', x') /= (y, x)]

change' arr y x = case place of
    TakenSeat -> if occupied >= 5 then FreeSeat else TakenSeat
    FreeSeat -> if occupied == 0 then TakenSeat else FreeSeat
    Floor -> Floor
  where
    occupied = length $ filter (== TakenSeat) $ map diag [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)]
    diag (addy, addx) = head $ dropWhile (== Floor) $ map (arr !) $  drop 1 $ iterate (\(y', x') -> (y' + addy, x' + addx)) (y, x)
    place = arr ! (y, x)