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
  if height' > 0 then do
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


-- Here is an example of a loop that will print the numbers from 1 to 10 in Haskell:

-- Copy code
-- main = do
--     -- Define a variable to hold the current number
--     let n = 1
--     -- Start a loop that will run until n is greater than 10
--     loop n

-- -- Define the loop function
-- loop n = do
--     -- Print the current number
--     putStrLn (show n)
--     -- Increment the number by 1
--     let n' = n + 1
--     -- If the new number is less than or equal to 10, call the loop function again
--     -- with the new number as the argument
--     if n' <= 10
--         then loop n'
--         -- If the new number is greater than 10, do nothing and the loop will end
--         else return ()