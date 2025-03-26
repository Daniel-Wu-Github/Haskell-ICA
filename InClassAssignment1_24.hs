{-Daniel Wu
    1/24/2025
    Functions and Lists
-}

{- Write the functions sumList
                       doubleSum
                       sumEvenNumbers
                       SumDoubleEvenNumbers
Then, use ghci to test each function
When ready, compile this program and upload
your code and paste your output to the questions below. -}

sumList :: [Int] -> Int
sumList [] = 0
sumList (head:tail) = head + sumList tail

doubleSum :: [Int] -> Int
doubleSum [] = 0
doubleSum n = 2 * sumList n

sumEvenNumbers :: [Int] -> Int
sumEvenNumbers [] = 0
sumEvenNumbers (head:tail) =
    if (head `mod` 2) == 0 then -- mod 2 to find if the number is even
        head + sumEvenNumbers tail
    else
        0 + sumEvenNumbers tail
        
{-
sumEvenNumbers :: [Int] -> Int
sumEvenNumbers xs = sum (fiter even xs)
-}

sumDoubleEvenNumbers :: [Int] -> Int
sumDoubleEvenNumbers [] = 0
sumDoubleEvenNumbers n = 2 * sumEvenNumbers n

main :: IO ()
main = do
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  print (sumList numbers)               -- Output: 55
  print (doubleSum numbers)             -- Output: 110
  print (sumEvenNumbers numbers)        -- Output: 30
  print (sumDoubleEvenNumbers numbers)  -- Output: 60