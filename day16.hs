import Data.List.Extra (splitOn, transpose)
import Data.List

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    print $ solve' input

data Input = Input { rules :: [(String, [(Int, Int)])]
                   , yourTicket :: [Int]
                   , nearbyTickets :: [[Int]] }
    deriving (Show)

parse input = Input rules yourTicket nearbyTickets where
    rules = map parseRule $ lines rulesStr
    [rulesStr, yourTicketStr, nearbyTicketsStr] = splitOn "\n\n" input
    yourTicket = parseTicket $ lines yourTicketStr !! 1
    nearbyTickets = map parseTicket $ drop 1 $ lines nearbyTicketsStr

parseRule rulesStr = (name, [(a, b), (c, d)]) where
    [a, b, c, d] = map read $ concat $ map (splitOn "-") $ splitOn " or "rangesStr :: [Int]
    [name, rangesStr] = splitOn ": " rulesStr

parseTicket s = map read $ splitOn "," s :: [Int]

solve (Input rules _ nearbyTickets) = sum $ filter (not . validForAnyRule rules) (concat nearbyTickets)

validForAnyRange ranges n = any (\(a, b) -> a <= n && n <= b) ranges where

validForAnyRule rules n = validForAnyRange (concat $ map snd rules) n

solve' (Input rules yourTicket nearbyTickets) = product $ map (yourTicket !!) departureIndexes where
    departureIndexes = map fst $ filter (isPrefixOf "departure" . snd) ([0..] `zip` columnsNames)
    columnsNames = filterRules validRulesForColumns
    validRulesForColumns = map (validRules rules) columns
    columns = transpose validTickets
    validTickets = filter (validTicket rules) nearbyTickets

validTicket rules ticket = all (validForAnyRule rules) ticket

validRules rules nums =  map fst $ filter (\(name, ranges) -> validRuleForAllNums ranges) rules where
    validRuleForAllNums ranges = all (validForAnyRange ranges) nums

filterRules rulesList = if length singleRules == length rulesList then singleRules else filterRules reducedRules where
    reducedRules = map removeRules rulesList
    singleRules = concat $ filter ((== 1) . length) rulesList
    removeRules rules = if length rules == 1 then rules else filter (not . (`elem` singleRules)) rules
