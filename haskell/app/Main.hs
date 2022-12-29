import Control.Monad (forM_, mapM_)
import System.Random ( mkStdGen, Random(randomRs) )
import Data.List ( transpose )



createColumn :: Int -> Int -> [Char]
createColumn position height =
  let spaces = replicate height ' '
      line = take (position - 1) spaces ++ "*" ++ drop position spaces
   in line

createXaxis :: Int -> String
createXaxis length =
  replicate length '-'

genColumns :: Int -> [Int] -> [[Char]]
genColumns height values = do
  let joinColumns x acc = x ++ [createColumn acc height]
  foldl joinColumns [] values


chart :: IO ()
chart = do
  let length = 20
  let height = 5
  let generator = mkStdGen 42
  let randomNumbers = take 10 (randomRs (1, height) generator)
  let columns = genColumns height randomNumbers
  let transposed = transpose columns
  let withYAxis = map ('|':) transposed

  let xAxis = " " ++ createXaxis length
  let chart = withYAxis ++ [xAxis]
  putStrLn "\n\n"
  putStrLn (unlines chart)

main :: IO ()
main = do
  chart
