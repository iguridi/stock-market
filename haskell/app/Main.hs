import Control.Monad (forM_)
import System.Random ( mkStdGen, Random(randomRs) )


createLine :: Int -> Int -> String
createLine position length =
  let spaces = replicate length ' '
      line = take (position - 1) spaces ++ "*" ++ drop position spaces
   in line

createXaxis :: Int -> String
createXaxis length =
  replicate length '-'

genLines :: Int -> [Int] -> [Char]
genLines length values = do
  let joinLines x acc = x ++ "\n|" ++ createLine acc length
  foldl joinLines "" values


chart :: IO ()
chart = do
  -- let position = 10
  let length = 20
  -- let array = [1, 2, 3, 4, 5]
  let generator = mkStdGen 42
  let randomNumbers = take 10 (randomRs (1, 5) generator)
  let lines = genLines length randomNumbers
  let xAxis = " " ++ createXaxis length
  putStrLn "\n\n"
  putStrLn lines
  putStrLn xAxis

main :: IO ()
main = do
  chart
