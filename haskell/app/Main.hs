import Data.List (transpose)
import System.Random (Random (randomRs), mkStdGen)

numberOfOffers :: Int
numberOfOffers = 10
-- TIME = 75

createColumn :: Int -> Int -> [Char]
createColumn value chartHeight =
  reverse [if y == value then '*' else ' ' | y <- [1 .. chartHeight]]


genColumns :: Int -> [Int] -> [[Char]]
genColumns chartHeight values = do
  let joinColumns x acc = x ++ [createColumn acc chartHeight]
  foldl joinColumns [] values


simulation = do
  let generator = mkStdGen 42
  let biding = take NUMBER_OF_ORDERS (randomRs (4, 25) generator)
  let asking = take NUMBER_OF_ORDERS (randomRs (25, 50) generator)


chart :: IO ()
chart = do
  let chartWidth = 10
  let chartHeight = 5

  let generator = mkStdGen 42
  -- let randomNumbers = take 10 (randomRs (1, chartHeight) generator)
  let randomNumbers = [1, 2, 3, 4, 4, 3, 2, 1, 5]

  let columns = genColumns chartHeight randomNumbers
  let transposed = transpose columns
  let withYAxis = map ('|' :) transposed

  let xAxis = replicate chartWidth '-'
  let chart = withYAxis ++ [xAxis]
  putStrLn "\n\n"
  putStrLn (unlines chart)

main :: IO ()
main = do
  chart
