createLine :: Int -> Int -> String
createLine position length =
  let spaces = replicate length ' '
      line = take (position - 1) spaces ++ "*" ++ drop position spaces
  in line

createXaxis :: Int -> String
createXaxis length =
  replicate length '-'


chart :: IO ()
chart = do
  let position = 10
  let length = 20
  let line = "|" ++ createLine position length
  let xAxis = " " ++ createXaxis length
  putStrLn line
  putStrLn xAxis


main :: IO ()
main = do
  chart