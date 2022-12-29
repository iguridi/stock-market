import Data.List (transpose)
import System.Random (Random (randomRs), mkStdGen)

numberOfOffers :: Int
numberOfOffers = 10

time :: Int
time = 75

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

simulation = do
  let generator = mkStdGen 42
  -- let biding = take NUMBER_OF_ORDERS (randomRs (4, 25) generator)
  -- let asking = take NUMBER_OF_ORDERS (randomRs (25, 50) generator)
  let history = replicate time (1, 50)
  -- let lastPrice = minimum asking
  -- let delta = 0

  -- turns
  -- graph
  chart history


chart :: [(Int, Int)] -> IO ()
chart history = do
  let chartHeight = maximum (map maximum history)
  let chartWidth = time

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
