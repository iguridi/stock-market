import Control.Monad -- (when, join)
import Data.List (transpose)
-- (Random (randomRs), mkStdGen, randomRIO)
import Debug.Trace
import System.Random

numberOfOffers :: Int
numberOfOffers = 10

totalTurns :: Int
totalTurns = 200

toChar :: Int -> (Int, Int) -> Char
toChar x (from, to) = case x of
  _
    | x > from && x < to -> '|'
    | x == from || x == to -> '+'
    | otherwise -> ' '

createColumn :: Int -> (Int, Int) -> [Char]
createColumn chartHeight range = reverse [toChar y range | y <- [1 .. chartHeight]]

genColumns :: Int -> [(Int, Int)] -> [[Char]]
genColumns chartHeight values = do
  let joinColumns x acc = x ++ [createColumn chartHeight acc]
  foldl joinColumns [] values

mapOnce :: (a -> Maybe a) -> [a] -> [a]
mapOnce _ [] = []
mapOnce f (x : xs) = case f x of
  Nothing -> x : mapOnce f xs
  Just y -> y : xs

replaceX :: [Int] -> Int -> Int -> [Int]
replaceX items old new = mapOnce check items
  where
    check item
      | item == old = Just new
      | otherwise = Nothing

replaceNth :: Int -> (a -> a) -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n f (x : xs)
  | n == 0 = f x : xs
  | otherwise = x : replaceNth (n - 1) f xs

updateHistory :: Int -> (Int, Int) -> (Int, Int)
updateHistory price (minHist, maxHist) = (min price minHist, max price maxHist)

sellMarketOrder :: ([Int], [Int], [(Int, Int)], Int, Int) -> Int -> Int -> ([Int], [Int], [(Int, Int)], Int, Int)
sellMarketOrder (sellOrders, buyOrders, history, lastPrice, delta) time spread = do
  let bidPrice = maximum buyOrders
  let buyOrders' = replaceX buyOrders bidPrice (minimum sellOrders + spread)
  let history' = replaceNth (time - 1) (updateHistory bidPrice) history
  (sellOrders, buyOrders', history', bidPrice, bidPrice - lastPrice)

buyMarketOrder :: ([Int], [Int], [(Int, Int)], Int, Int) -> Int -> Int -> ([Int], [Int], [(Int, Int)], Int, Int)
buyMarketOrder (sellOrders, buyOrders, history, lastPrice, delta) time spread = do
  let askPrice = minimum sellOrders
  let sellOrders' = replaceX sellOrders askPrice (max (maximum buyOrders - spread) 1)
  let history' = replaceNth (time - 1) (updateHistory askPrice) history
  (sellOrders', buyOrders, history', askPrice, askPrice - lastPrice)

-- TODO: this is too much, use a monad?
oneTurn :: ([Int], [Int], [(Int, Int)], Int, Int) -> (Int, Bool, Int) -> ([Int], [Int], [(Int, Int)], Int, Int)
oneTurn info turn =
  trace
    ("DEBUG: oneTurn  \n" ++ show info ++ "\n" ++ show turn ++ "\n")
    ( do
        let (_, _, _, _, delta) = info
        let (time, random, spread) = turn
        -- TODO: spread is the same for all moves on each turn
        let tendency = if delta < 0 then buyMarketOrder else sellMarketOrder
        let randomDecision = if random then sellMarketOrder else buyMarketOrder
        let decisions = [randomDecision, tendency, tendency]
        foldl (\x y -> y x time spread) info decisions
    )

randomSequence :: Int -> Int -> Int -> IO [Int]
randomSequence total from to = replicateM total $ randomRIO (from, to :: Int)

chart :: [(Int, Int)] -> IO ()
chart history = do
  let columns = genColumns (maximum (map maximum history)) history
  let withYAxis = map ('|' :) (transpose columns)
  let chart = withYAxis ++ [replicate totalTurns '-']
  putStrLn "\n\n"
  putStrLn (unlines chart)

simulation :: IO ()
simulation = do
  buyOrders <- randomSequence numberOfOffers 1 25
  sellOrders <- randomSequence numberOfOffers 25 50
  randomSpreads <- randomSequence totalTurns 1 10
  let history = replicate totalTurns (50, 1)
  let info = (sellOrders, buyOrders, history, minimum buyOrders, 0)
  let randomnActions = take totalTurns $ randoms (mkStdGen 11) :: [Bool]
  let turns = zip3 [1 .. totalTurns] randomnActions randomSpreads
  let (_, _, history', _, _) = foldl oneTurn info turns
  chart history'

main :: IO ()
main = do
  simulation
