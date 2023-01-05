import Control.Monad --(when, join)
import Data.List (transpose)
import System.Random --(Random (randomRs), mkStdGen, randomRIO)
import Debug.Trace


numberOfOffers :: Int
numberOfOffers = 10

totalTurns :: Int
totalTurns = 75

toChar :: Int -> (Int, Int) -> Char
toChar x range = case x of
  _
    | x > fst range && x < snd range -> '|'
    | x == fst range || x == snd range -> '+'
    | otherwise -> ' '

createColumn :: Int -> (Int, Int) -> [Char]
createColumn chartHeight range =
  reverse [toChar y range | y <- [1 .. chartHeight]]

genColumns :: Int -> [(Int, Int)] -> [[Char]]
genColumns chartHeight values = do
  let joinColumns x acc = x ++ [createColumn chartHeight acc]
  foldl joinColumns [] values

mapOnce :: (a -> Maybe a) -> [a] -> [a]
mapOnce _ []     = []
mapOnce f (x:xs) = case f x of
        Nothing -> x : mapOnce f xs
        Just y  -> y : xs

replaceX :: [Int] -> Int -> Int -> [Int]
replaceX items old new = mapOnce check items where
    check item  | item == old = Just new
                | otherwise   = Nothing

replaceNth :: Int -> (a -> a) -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n f (x:xs)
  | n == 0 = f x:xs
  | otherwise = x:replaceNth (n-1) f xs


updateHistory :: Int -> (Int, Int) -> (Int, Int)
updateHistory price (minHist, maxHist) = (min price minHist, max price maxHist)


bid :: ([Int], [Int], [(Int, Int)], Int, Int) -> Int -> Int -> ([Int], [Int], [(Int, Int)], Int, Int)
bid (biding, asking, history, lastPrice, delta) time margin =
  -- trace ("DEBUG: bid  \n" ++ show biding ++ "\n" ++ show asking ++ "\n" ++ show history ++ "\n" ++ show lastPrice ++ "\n" ++ show delta ++ "\n\n") (
    do
  let price = minimum asking
  let asking' = replaceX asking price (margin + maximum biding)
  let history' = replaceNth time (updateHistory price) history
  (biding, asking', history', price, price - lastPrice)
  -- )

ask :: ([Int], [Int], [(Int, Int)], Int, Int) -> Int -> Int -> ([Int], [Int], [(Int, Int)], Int, Int)
ask (biding, asking, history, lastPrice, delta) time margin =
  -- trace ("DEBUG: ask  \n" ++ show biding ++ "\n" ++ show asking ++ "\n" ++ show history ++ "\n" ++ show lastPrice ++ "\n" ++ show delta ++ "\n" ++ show margin ++ "\n\n") (
    do
  let price = minimum biding
  let biding' = replaceX biding price (margin + minimum asking)
  let history' = replaceNth time (updateHistory price) history
  (biding', asking, history', price, price - lastPrice)
  -- )


oneTurn :: ([Int], [Int], [(Int, Int)], Int, Int) -> (Int, Bool, Int) -> ([Int], [Int], [(Int, Int)], Int, Int)
oneTurn info turn =
  -- trace ("DEBUG: oneTurn  \n" ++ show info ++ "\n" ++ show turn ++ "\n") (
    do
      let (_, _, _, _, delta) = info
      let (time, random, margin) = turn
      let randomDecision = if random then ask else bid
      let info' = randomDecision info time margin
      let tendency = if delta > 0 then ask else bid
      let info'' = tendency info' time margin
      let info''' = tendency info'' time margin
      info'''
  -- )

randomSequence :: Int -> Int -> Int -> IO [Int]
randomSequence total from to = replicateM total  $ randomRIO (from, to :: Int)

simulation :: IO ()
simulation = do
  let biding = take numberOfOffers (randomRs (1, 25) (mkStdGen 42))
  let asking = take numberOfOffers (randomRs (25, 50) (mkStdGen 41))
  -- biding <- randomSequence numberOfOffers 1 25
  -- asking <- randomSequence numberOfOffers 25 50
  let info = (biding, asking, replicate totalTurns (50, 1), minimum asking, 0)

  -- turns
  let randomnActions = take totalTurns $ randoms (mkStdGen 11) :: [Bool]
  let randomnMargins = take numberOfOffers (randomRs (0, 10) (mkStdGen 41))
  -- randomMargins <- randomSequence numberOfOffers 25 50
  let turns = zip3 [1 .. totalTurns] randomnActions randomnMargins
  let (biding', asking', history', lastPrice', delta') = foldl oneTurn info turns
  -- graph
  chart history'

chart :: [(Int, Int)] -> IO ()
chart history = do
  let chartHeight = maximum (map maximum history)
  let chartWidth = totalTurns

  let columns = genColumns chartHeight history
  let transposed = transpose columns
  let withYAxis = map ('|' :) transposed

  let xAxis = replicate chartWidth '-'
  let chart = withYAxis ++ [xAxis]
  putStrLn "\n\n"
  putStrLn (unlines chart)

main :: IO ()
main = do
  simulation
