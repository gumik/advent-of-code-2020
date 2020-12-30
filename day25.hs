main = do
    let cardPubKey = 1717001
        doorPubKey = 523731
        cardLoop = loopValue 7 cardPubKey
        doorLoop = loopValue 7 doorPubKey
    print $ transform cardPubKey doorLoop
    print $ transform doorPubKey cardLoop
    
loopValue subj key = fst $ head $ dropWhile ((/=key) . snd) $ zip [0..] (transforms subj)

transform subj loop = head $ drop loop $ transforms subj

transforms subj = iterate (transformStep subj) 1

transformStep subj n = (subj * n) `mod` 20201227