createLine :: Int -> Int -> String
createLine position length =
  let spaces = replicate length ' '
      line = take (position - 1) spaces ++ "*" ++ drop position spaces
  in line

createXaxis :: Int -> String
createXaxis length =
  replicate length '-'

main :: IO ()
main = do
  let line = "|" ++ createLine 10 20
  let xAxis = " " ++ createXaxis 10
  putStrLn  line
  putStrLn xAxis