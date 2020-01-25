module EulerLib where

import Data.List(unfoldr)

eqDiv :: Integral a => a -> a -> Bool
eqDiv dividend divisior = dividend `mod` divisior == 0

notEqDiv :: Integral a => a -> a -> Bool
notEqDiv dividend divisior = dividend `mod` divisior /= 0

fib :: Integral a => [a]
fib = 1 : scanl (+) 1 fib

fib' :: Integer -> Integer
fib' n 
  | n < 2    = n
  |otherwise = fib' (n-1) + fib' (n-2)

fibZip :: Integral a => [a]
fibZip = 0 : 1 : zipWith (+) fibZip (tail fibZip)
  -- zipWith (+) 
      -- fibZip       = 0 : 1 : ....
      -- tail fibZip  = 1 : 1 : ...
      --       0 : 1  = 1 : 2 : ...

fibAnamorphism :: Integral a => [a]
fibAnamorphism = unfoldr (\(a,b) -> let fibNum = a+b in Just (fibNum, (b,fibNum))) (0,1)

fibAnamorphism' :: Integral a => [a]
fibAnamorphism' = fst <$> iterate (\(a,b) -> (b,a+b)) (0,1)


isPrime :: Integral a => a -> Bool
isPrime n = (n >= 2) && go n 2
  where go n' divisor
          | n' `eqDiv` divisor = divisor == n'
          | otherwise          = go n' $ divisor + 1

primeFactors :: Integral a => a -> [a]
primeFactors n = go n 2
  where 
    go dividend divisor
      | dividend < 2   = []
      | remainder == 0 = divisor : go quotient divisor
      | otherwise      = go dividend $ divisor + increment
      where 
        (quotient, remainder) = dividend `divMod` divisor
        increment | divisor == 2 = 1
                  | otherwise = 2

numToDigits' :: Integral a => a -> [a]
numToDigits' 0   = [0]
numToDigits' num = go num []
  where 
    go 0 _  = []
    go n xs = let (quotient, remainder) = n `divMod` 10 
              in  if (quotient == 0) then remainder : xs
                  else go quotient $ remainder : xs

numToDigits :: Integral a => a -> [a]
numToDigits 0   = [0]
numToDigits num = go num []
  where 
    go 0 _  = []
    go n xs = case n `divMod` 10 of
                (0,        remainder) -> remainder : xs
                (quotient, remainder) -> go quotient $ remainder : xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

isNumPalindrome :: Integral a => a -> Bool
isNumPalindrome = isPalindrome . numToDigits


-- lcm of 3 and 5 is 15... 
lowestCommonMultiplier :: Integral a => a -> a -> a
lowestCommonMultiplier a b = go max
  where 
    go ans =  if (ans `eqDiv` min) then ans 
              else go (ans + max) 
    (max, min) = if (a>b) then (a,b) else (b,a) 

lcm' :: Integral a => a -> a -> a
lcm' = lowestCommonMultiplier
  