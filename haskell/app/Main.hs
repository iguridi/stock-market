import Control.Monad (forM_, mapM_)
import System.Random ( mkStdGen, Random(randomRs) )
import Data.List



createColumn :: Int -> Int -> [Char]
createColumn position length =
  let spaces = replicate length ' '
      line = take (position - 1) spaces ++ "*" ++ drop position spaces
   in line

createXaxis :: Int -> String
createXaxis length =
  replicate length '-'

genColumns :: Int -> [Int] -> [[Char]]
genColumns length values = do
  let joinColumns x acc = x ++ [createColumn acc length]
  foldl joinColumns [] values


chart :: IO ()
chart = do
  -- let position = 10
  let length = 20
  -- let array = [1, 2, 3, 4, 5]
  let generator = mkStdGen 42
  let randomNumbers = take 10 (randomRs (1, 5) generator)
  let columns = genColumns length randomNumbers
  let transposed = transpose columns

  let xAxis = " " ++ createXaxis length
  putStrLn "\n\n"
  mapM_ print transposed
  putStrLn xAxis

main :: IO ()
main = do
  chart
