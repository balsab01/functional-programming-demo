-- Feel free to experiment with Haskell in this file!
-- This will not affect your assignment's grade

addNums :: Int -> Int
addNums 0 = 0
addNums n = n + addNums (n - 1)

main = print (addNums 10)