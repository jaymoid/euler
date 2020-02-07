module Q1to10 where

import EulerLib
import Data.Function ((&), on)
import Data.List (maximumBy, null)
import qualified Data.List.NonEmpty as NonEmpty 
import Data.Maybe (catMaybes)
import Data.Semigroup (Product(..), Max(..), sconcat)
import Control.Monad (guard)
import Control.Applicative (liftA2)

-- Q1
sumOfThings :: Integral b => b -> b
sumOfThings n = [1..(n-1)] & filter (\x -> eqDiv x 5 || eqDiv x 3) & sum

sumOfThings' :: Integral b => b -> b
sumOfThings' n = sum [ x | x <- [1..(n-1)], x `eqDiv` 5 || x `eqDiv` 3]

sumOfThings'' :: Integral b => b -> b
sumOfThings'' n = sum $ filter (\x -> eqDiv x 5 || eqDiv x 3) $ [1..(n-1)]


-- Q2
-- sumOfFibUnder :: Integer -> Integer
sumOfFibUnder :: Integral a => a -> a
sumOfFibUnder n = sum $ filter (\x -> eqDiv x 2) $ takeWhile ((>) n) $ drop 1 fib


-- Q3
{- 
  Prime numbers: 
  Are divisible only by themselves and 1

  Prime factors of a number:
  Are the prime numbers that when multiplied together, equal the number in question

  The prime factor of a prime number is n

  I had to read this to understand prime factorisation...
  https://www.bbc.co.uk/bitesize/guides/z9hb97h/revision/4

  example from website (but starting to divide by 2 not 4):

  n   divisor     quotient  remainder
  40    2      =    20          0        <- as remainder is 0, first prime factor found (2)
  20    2      =    10          0        <- as remainder is 0, prime factor found (2)
  10    2      =    5           0        <- as remainder is 0, prime factor found (2)
  5     2      =    2           1        <- as remainder is not 0 start again...with divisor + 1 
  5     3      =    1           2        <- as remainder is not 0 start again...with divisor + 1 
  5     4      =    1           1        <- as remainder is not 0 start again...with divisor + 1 
  5     5      =    1           0        <- as remainder is 0, prime factor found (5)

  Therefore:
  2 x 2 x 2 x 5 = 40

-}

-- First go
maxPrimeFactor :: Integer -> Maybe Integer
maxPrimeFactor n | n < 2     = Nothing
                 | otherwise = go n 2
  where
    go dividend divisor 
      | dividend < 2     = Just divisor
      | otherwise = let (quotient, remainder) = dividend `divMod` divisor
                    in if remainder /= 0 then 
                          go dividend $ divisor + 1
                       else 
                          go quotient divisor
             
-- Tidied version of above (0.0170...seconds)
maxPrimeFactor' :: Integer -> Maybe Integer
maxPrimeFactor' n = if n < 2 then Nothing else go n 2
  where 
    go dividend divisor | dividend < 2             = Just divisor
                        | dividend `eqDiv` divisor = go (dividend `div` divisor) divisor
                        | otherwise                = go dividend $ divisor + 1

-- The divMod optimisation + increment calc, makes this run in (0.0091 seconds)
maxPrimeFactorOptimised :: Integer -> Maybe Integer
maxPrimeFactorOptimised n = if n < 2 then Nothing else go n 2
  where 
    go dividend divisor
      | dividend < 2   = Just divisor
      | remainder == 0 = go quotient divisor
      | otherwise      = go dividend $ divisor + increment
      where 
        (quotient, remainder) = dividend `divMod` divisor
        increment | divisor == 2 = 1
                  | otherwise    = 2


-- Q4

-- First go... Max of a list comprehension
biggestPalindromeProductOfV1 :: Integral a => a -> (a, a, a)
biggestPalindromeProductOfV1 n = 
  maximumBy (\(_, _, a) (_, _, b) -> compare a b)
  [(x, y, z) | x <- [1..n], y <- [1..n], z <- [x * y], isNumPalindrome z] 

-- unsing compare `on`
biggestPalindromeProductOfV2 :: Integral a => a -> (a, a, a)
biggestPalindromeProductOfV2 n = 
  maximumBy (compare `on` (\(_,_,x) -> x))
  [(x, y, z) | x <- [1..n], y <- [1..n], z <- [x * y], isNumPalindrome z] 

-- Do you even lift (...functions over applicative structure) bro?
biggestPalindromeProductOfV3 :: Integral a => a -> (a, a, a)
biggestPalindromeProductOfV3 n = 
  maximumBy biggestPalindrome $ catMaybes $ toPalindrome <$> [1..n] <*> [1..n]
  where
    biggestPalindrome = compare `on` (\(_,_,x) -> x)
    toPalindrome x y = let res = x * y in if isNumPalindrome res then Just (x,y,res) else Nothing

-- [99..1] ???
biggestPalindromeProductOfV4 :: Integral a => a -> (a, a, a)
biggestPalindromeProductOfV4 n = maximumBy 
  (\(_, _, a) (_, _, b) -> compare a b)
  [(x, y, z) | x <- nnn, y <- nnn, z <- [x * y], isNumPalindrome z] 
  where
    nnn = enumFromThenTo n (n-1) 1

-- ONE QUESTION TWO SEMIGROUPS!
q4 :: Integral a => a -> a
q4 n =  getMax . sconcat . NonEmpty.fromList
              $ fmap Max
              $ filter isNumPalindrome
              $ fmap (getProduct. mconcat . fmap Product) 
              $ (\x -> (:[x])) <$> [1..n] <*> [1..n]

-- shortest? if you only care about the answer, not the factors...
q4' :: Integral a => a -> a
q4' n = maximum $ filter isNumPalindrome $ liftA2 (*) [1..n] [1..n]

-- Some shit playing around with List monad/applicative, all the following are equivalent
combo, comboBind, comboDo :: Integral c => c -> [(c, c, c)]
combo n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [x*y], isNumPalindrome z] 

comboDo n = do
  x <- [1..n]
  y <- [1..n]
  z <- [x * y]
  _ <- guard (isNumPalindrome z)
  return (x, y, z)

comboBind n = 
  [1..n] >>= (\x ->
    [1..n] >>= (\y ->
      [x * y] >>= (\z ->
        guard (isNumPalindrome z) >>= (\_ ->
          return (x,y,z)
        )
      )
    )
  )




-- Q5

-- This was my first go, it seems this is very slow at finding the [1..20] answer :)
smallestNumberThatCanBeDividedByNaive :: (Integral a, Show a) => [a] -> a
smallestNumberThatCanBeDividedByNaive xs = go $ maximum xs
  where go n = if all (\x -> n `eqDiv` x) xs then n else go n+1


smallestNumberThatCanBeDividedBy :: (Integral a, Show a) => [a] -> a
smallestNumberThatCanBeDividedBy nums = 
  primeFactors <$> nums 
  & filter (not . null) 
  & fmap productOfMaxPrimes 
  & removeDuplicatePrimes
  & product
  where
    productOfMaxPrimes xs = let maxPrime = maximum xs 
                            in  product $ filter ((==) maxPrime) xs
    removeDuplicatePrimes []     = []
    removeDuplicatePrimes (x:xs) = if any (\y -> y >= x && y `eqDiv` x) xs then removeDuplicatePrimes xs
                          else x : removeDuplicatePrimes xs

q5r :: (Integral a, Show a) => [a] -> a
q5r =  foldr1 lowestCommonMultiplier 

q5l :: (Integral a, Show a) => [a] -> a
q5l = foldl1 lowestCommonMultiplier

lcmUsingPrimes :: Integral a => [a] -> [a]
lcmUsingPrimes = foldl cmp [] 
  where 
    cmp :: Integral a => [a] -> a -> [a]
    cmp acc b = acc ++ [(productOfMaxPrimes (primeFactors b))]
      -- (productOfMaxPrimes (primeFactors a)) * (productOfMaxPrimes (primeFactors b))
    productOfMaxPrimes :: Integral a => [a] -> a
    productOfMaxPrimes xs = let maxPrime = maximum xs 
                            in  product $ filter ((==) maxPrime) xs


-- Q6 
-- The sum of the squares of the first ten natural numbers is =385
-- The square of the sum of the first ten natural numbers is =3025

-- Hence the difference between the sum of the squares of the first ten natural 
-- numbers and the square of the sum is 3025−385=2640

-- Find the difference between the sum of the squares of the first one hundred 
-- natural numbers and the square of the sum.

sumOfSquaresOf :: Integral a => [a] -> a
sumOfSquaresOf xs = sum $ (\x -> x ^ (2 :: Integer)) <$> xs

squareOfTheSumOf :: Integral a => [a] -> a
squareOfTheSumOf xs = (\sum' -> sum' ^ (2 :: Integer)) $ sum xs

diffSumOfSquaresAndSquareOfSums :: Integral a => [a] -> a
diffSumOfSquaresAndSquareOfSums xs = (squareOfTheSumOf xs) - (sumOfSquaresOf xs) 


-- Q7
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
-- that the 6th prime is 13.

-- What is the 10 001st prime number?
nthPrime :: Integral a => a -> a
nthPrime n = last $ take (fromIntegral n) trialDivisionPrimes

trialDivisionPrimes :: Integral a => [a]
trialDivisionPrimes = catMaybes $ maybePrime <$> [1..]
  where maybePrime x = if isPrime x then Just x else Nothing

-- this algorithm is simple but has flaws:
-- It includes many numbers that can be ruled out (because divisible by 2, 3, 5, or 7)
-- When it finds a prime it calculates if it is prime...
-- This is approach is known as using trial division (see how isPrime is implemented)
-- so this is kinda slow, so looking at 
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

-- From... 
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
-- copy of this doc in the materials folder in this project.
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
    sieve [] = undefined 

nthPrimeSeive :: Integer -> Integer
nthPrimeSeive n = last $ take (fromIntegral n) primes

-- I am using this as a comparison 
