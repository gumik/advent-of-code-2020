import Text.Printf

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input

parse = map (\[cmd, n] -> (cmd, readInt n)) . map words . lines
readInt str = read $ filter (/= '+') str :: Int

-- This is soooo UGLY and OVERCOMPLICATED! Needs to be reworked.
solve input = getAccumulator $ head $ drop (length steps -1 ) steps where
    steps = takeWhile (\(_, (_, _, count), _, _) -> count >= 0) $ iterate step ([], head inputWithCounts, tail inputWithCounts, 0)
    addCount (cmd, n) = (cmd, n, 0)
    inputWithCounts = map addCount input
    step (_, (_, _, 1), _, accumulator) = ([], ("", 0, -1), [], accumulator)
    step (prev, (cmd, n, count), next, accumulator) = case cmd of
        "nop" -> (prev ++ [(cmd, n, count+1)], head next, tail next, accumulator)
        "acc" -> (prev ++ [(cmd, n, count+1)], head next, tail next, accumulator+n)
        "jmp" -> if n > 0
            then (prev ++ [(cmd, n, count+1)] ++ take (n-1) next, head $ drop (n-1) next, drop (n) next, accumulator)
            else (take (length prev + n) prev, head $ drop (length prev + n) prev, drop (length prev + n + 1) prev ++ [(cmd, n, count+1)] ++ next, accumulator)
    getAccumulator (_, (_, _, _), _, accumulator) = accumulator