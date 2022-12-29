import Control.Monad (forM_, mapM_)
import Data.List (transpose)
import System.Random (Random (randomRs), mkStdGen)

createColumn :: Int -> Int -> [Char]
createColumn columnPosition chartHeight =
  reverse [if y == columnPosition then '*' else ' ' | y <- [1 .. chartHeight]]


createXaxis :: Int -> String
createXaxis chartWidth =
  replicate chartWidth '-'

genColumns :: Int -> [Int] -> [[Char]]
genColumns chartHeight values = do
  let joinColumns x acc = x ++ [createColumn acc chartHeight]
  foldl joinColumns [] values

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

  let xAxis = " " ++ createXaxis chartWidth
  let chart = withYAxis ++ [xAxis]
  putStrLn "\n\n"
  putStrLn (unlines chart)

main :: IO ()
main = do
  chart
