createLine :: Int -> Int -> String
createLine position length =
  let spaces = replicate length ' '
      line = take (position - 1) spaces ++ "*" ++ drop position spaces
   in line

createXaxis :: Int -> String
createXaxis length =
  replicate length '-'

loop :: Int -> Int -> Int -> IO [Char]
loop length height position = do
  let height' = height - 1
  if height' > 0
    then do
      let line = "\n|" ++ createLine position length
      other_lines <- loop length height' position
      return (line ++ other_lines)
    else return ""

chart :: IO ()
chart = do
  let position = 10
  let length = 20
  lines <- loop length 10 position
  let xAxis = " " ++ createXaxis length
  putStrLn "\n\n"
  putStrLn lines
  putStrLn xAxis

main :: IO ()
main = do
  chart
