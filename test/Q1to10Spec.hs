module Q1to10Spec where

import Test.Hspec

import Q1to10

testQ1to10 :: IO ()
testQ1to10 = hspec $ do

  -- describe "Q1" $ do
  --   it "example" $ do
  --     sumOfThings 10 `shouldBe` 23
  --   it "answer" $ do
  --     sumOfThings 1000 `shouldBe` 233168

  -- describe "Q2" $ do
  --   it "answer" $ do
  --     sumOfFibUnder 4000000 `shouldBe` 4613732

  -- describe "Q3" $ do
  --   it "tdding it!" $ do
  --     maxPrimeFactor 0 `shouldBe` Nothing
  --     maxPrimeFactor 1 `shouldBe` Nothing
  --     maxPrimeFactor 2 `shouldBe` Just 2
  --     maxPrimeFactor 3 `shouldBe` Just 3
  --     maxPrimeFactor 4 `shouldBe` Just 2
  --     maxPrimeFactor 5 `shouldBe` Just 5
  --     maxPrimeFactor 6 `shouldBe` Just 3
  --     maxPrimeFactor 7 `shouldBe` Just 7
  --     maxPrimeFactor 8 `shouldBe` Just 2
  --   it "sample" $ 
  --     maxPrimeFactor 13195 `shouldBe` Just 29
  --   it "answer" $ do
  --     maxPrimeFactor 600851475143 `shouldBe` Just 6857

  -- describe "Q4" $ do
  --   it "Sample" $ 
  --     biggestPalindromeProductOfV1 99 `shouldBe` (99, 91, 9009)
  --   it "answer" $ 
  --     biggestPalindromeProductOfV1 999 `shouldBe` (993, 913, 906609)

  -- describe "Q5" $ do
  --   it "Sample" $ 
  --     smallestNumberThatCanBeDividedBy [1..10] `shouldBe` 2520
  --   it "answer" $ 
  --     smallestNumberThatCanBeDividedBy  [1..20] `shouldBe` 232792560

  -- describe "Q6" $ do
  --   it "Sample: Sum of squares of first ten numbers = 385" $ 
  --     sumOfSquaresOf [1..10] `shouldBe` 385
  --   it "Sample: The square of the sum of the first ten natural numbers is = 3025" $
  --     squareOfTheSumOf [1..10] `shouldBe` 3025
  --   it "Sample: Difference between sum of the squares of the first ten natural \
  --      \ numbers and the square of the sum is= 3025−385 = 2640" $
  --     diffSumOfSquaresAndSquareOfSums [1..10] `shouldBe` 2640 
  --   it "Answer: Find the difference between the sum of the squares of the first\
  --      \ one hundred natural numbers and the square of the sum. " $ 
  --     diffSumOfSquaresAndSquareOfSums [1..100] `shouldBe` 25164150
  
  -- describe "Q7" $ do
  --   it "Sample: The first 6 primes are [2, 3, 5, 7, 11, 13]" $ do
  --     nthPrime 1 `shouldBe` 2
  --     nthPrime 2 `shouldBe` 3
  --     nthPrime 3 `shouldBe` 5
  --     nthPrime 4 `shouldBe` 7
  --     nthPrime 5 `shouldBe` 11
  --     nthPrime 6 `shouldBe` 13
  describe "Q7" $ do
    it "Answer: the 10,001th prime is 104743" $
      nthPrimeSeive 10001 `shouldBe` 104743