import Text.Printf
import Data.List

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    print $ solve' input

parse = map ((\[cmd, n] -> (cmd, readInt n)) . words) . lines
readInt str = read $ filter (/= '+') str :: Int

data State = State { position :: Int
                   , accumulator :: Int
                   , passedPositions :: [Int] }
           | Finished
    deriving (Show)
isFinished state = case state of
    Finished -> True
    _ -> False

solve input = accumulator $ last steps where
    steps = takeWhile noDuplicates $ runCode input
    noDuplicates state = length (nub pos) == length pos where
        pos = passedPositions state

solve' input = accumulator $ last $ takeWhile (not . isFinished) finishedComputation where
    finishedComputation = head $ filter (isFinished . last) $ map (take (length input) . runCode) possibleFixes
    possibleFixes = map changeInstruction jmpOrNopPositions  -- brute force :)
    jmpOrNopPositions = map fst $ filter ((`elem` ["jmp", "nop"]) . fst . snd) $ zip [0,1..] input
    changeInstruction pos = part1 ++ ((newCmd, n):part2) where
        (part1, (cmd, n):part2) = splitAt pos input
        newCmd = case cmd of
            "nop" -> "jmp"
            "jmp" -> "nop"

runCode input = iterate (step input) $ State 0 0 [] where
    step _ Finished = Finished
    step input state = if pos >= length input
        then Finished
        else case cmd of
            "nop" -> state { passedPositions = pos:passed, position = pos + 1 }
            "acc" -> state { passedPositions = pos:passed, position = pos + 1, accumulator = acc + val }
            "jmp" -> state { passedPositions = pos:passed, position = pos + val }
          where
            State pos acc passed = state
            (cmd, val) = input !! pos
