import Control.Monad (when)
import Data.List (transpose)
import System.Random (Random (randomRs), mkStdGen)

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

oneTurn :: ([Int], [Int], [(Int, Int)], Int, Int) -> Int -> ([Int], [Int], [(Int, Int)], Int, Int)
oneTurn (biding, asking, history, lastPrice, delta) time = (biding, asking, history, lastPrice, delta)

makeOffers :: (Int, Int) -> [Int]
makeOffers range = do
  let generator = mkStdGen 42
  take numberOfOffers (randomRs range generator)

simulation = do
  let biding = makeOffers (1, 25) --take numberOfOffers (randomRs (1, 25) generator)
  let asking = makeOffers (25, 50) --take numberOfOffers (randomRs (25, 50) generator)
  let history = replicate totalTurns (1, 50)
  let lastPrice = minimum asking
  let delta = 0

  -- turns
  let (biding', asking', history', lastPrice', delta') = foldl oneTurn (biding, asking, history, lastPrice, delta) [1 .. totalTurns]
  -- graph
  chart history'

chart :: [(Int, Int)] -> IO ()
chart history = do
  let chartHeight = maximum (map maximum history)
  let chartWidth = totalTurns

  -- let generator = mkStdGen 42
  -- let randomNumbers = take 10 (randomRs (1, chartHeight) generator)
  -- let randomNumbers = [1, 2, 3, 4, 4, 3, 2, 1, 5]

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
