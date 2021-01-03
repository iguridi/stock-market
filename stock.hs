offerMargin :: Int
offerMargin = 4


deleteFirst _ [] = []
deleteFirst a (b:bc) | a == b    = bc
                     | otherwise = b : deleteFirst a bc


addToList :: Int -> [Int] -> [Int]
addToList a [] = [a]
addToList a (x:xs) = x : addToList a xs


addBuyOffer :: [Int] -> Int -> [Int]
addBuyOffer buying price = addToList x buying where
    x = max (price - offerMargin) 1


addSellOffer :: [Int] -> Int -> [Int]
addSellOffer selling price = addToList x selling where
    x = price + offerMargin

-- exchange :: [Int] -> [Int] -> ([Int] -> Int -> [Int]) -> (a -> a -> a) -> (a -> a -> a) -> ([Int], [Int], no se que va acá)

main = do
    let res = offerMargin
    putStrLn $ "1+2+3 = " ++ show res

    let res = deleteFirst 4 [1..10]
    putStrLn $ "no 4 = " ++ show res

    let res = addBuyOffer [1..5] 6
    putStrLn $ "buy offer = " ++ show res

    let res = addSellOffer [1..5] 6
    putStrLn $ "sell offer = " ++ show res
