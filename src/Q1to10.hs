module Q1to10 where

import Lib
import Data.Function ((&))

-- Q1
sumOfThings n = [1..(n-1)] & filter (\x -> eqDiv x 5 || eqDiv x 3) & sum

sumOfThings' n = sum [ x | x <- [1..(n-1)], x `eqDiv` 5 || x `eqDiv` 3]

sumOfThings'' n = sum $ filter (\x -> eqDiv x 5 || eqDiv x 3) $ [1..(n-1)]


-- Q2
sumOfFibUnder n = sum $ filter (\x -> eqDiv x 2) $ takeWhile ((>) n) $ drop 1 fib


-- Q3
maxPrimeFactor' :: Integer -> Maybe Integer
maxPrimeFactor' n | n < 2     = Nothing
                  | otherwise = go n 2
  where
    go n divisor 
      | (n < 2)   = Just divisor
      | otherwise = let (quotient, remainder) = n `divMod` divisor
                    in if (remainder /= 0) then 
                          go n $ divisor + 1
                       else 
                          go quotient divisor
             
-- tidied version of above
maxPrimeFactor :: Integer -> Maybe Integer
maxPrimeFactor n = if n < 2 then Nothing else go n 2
  where go n divisor | n < 2                = Just divisor
                     | n `eqDiv` divisor    = go (n `div` divisor) divisor
                     | otherwise            = go n $ divisor + 1

