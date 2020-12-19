import Data.Char (isDigit)

main = do
    cont <- getContents
    let input = parse cont
    print $ addPrecedence [] $ head input
    print $ solve input
    print $ solve' input

data Token = Number Int | OpPlus | OpMult | LeftParen | RightParen deriving (Show, Eq)
getNum (Number n) = n
parse = map (tokenize . filter (/= ' ')) . lines

tokenize [] = []
tokenize s = let (token, rest) = readToken s in token:tokenize rest

readToken s@(x:xs)
    | isDigit x = readInt s
    | x == '+'  = (OpPlus, xs)
    | x == '*'  = (OpMult, xs)
    | x == '('  = (LeftParen, xs)
    | x == ')'  = (RightParen, xs)
   
readInt s = let (numStr, rest) = span isDigit s in (Number $ read numStr, rest)

solve = sum . map (getNum . head . fst . calc [])

calc st [] = (st, [])
calc st (token:rest) = case token of
    Number n -> case st of
        [] -> calc [Number n] rest
        (op:Number m:st') -> calc ((Number $ applyOp op m n):st') rest
    OpPlus -> calc (OpPlus:st) rest
    OpMult -> calc (OpMult:st) rest
    LeftParen -> let ([num], rest') = calc [] rest in calc st (num:rest')
    RightParen -> (st, rest)

applyOp op a b = case op of
    OpPlus -> a + b
    OpMult -> a * b

solve' = solve . map (addPrecedence [])

addPrecedence prev [] = reverse prev
addPrecedence prev (x:xs) = case x of
    OpPlus -> addPrecedence (x:prev') xs' where
        prev' = (addParen LeftParen RightParen 0 prev)
        xs' = (addParen RightParen LeftParen 0 xs)
    _ -> addPrecedence (x:prev) xs

addParen paren other 0 [] = [paren]
addParen paren other depth (x:xs) = if depth == 0
    then case x of
        Number _ -> x:paren:xs
        OpMult -> paren:x:xs
        OpPlus -> paren:x:xs
        x -> if x == paren
            then paren:paren:xs
            else x:(addParen paren other 1 xs)
    else if x == other
        then x:(addParen paren other (depth+1) xs)
        else if x == paren then x:(addParen paren other (depth-1) xs)
        else x:(addParen paren other depth xs)
