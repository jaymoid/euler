module EulerLibSpec where

import Test.Hspec
import EulerLib

libTest :: IO ()
libTest = hspec $ do

  describe "isPrime" $ do
    it "notPrimes!" $ do
      isPrime' 0 `shouldBe` False
      isPrime' 1 `shouldBe` False
      isPrime' 4 `shouldBe` False
      isPrime' 6 `shouldBe` False
      isPrime' 8 `shouldBe` False
      isPrime' 9 `shouldBe` False
    it "primes" $ do
      isPrime' 2 `shouldBe` True
      isPrime' 3 `shouldBe` True
      isPrime' 5 `shouldBe` True
      isPrime' 7 `shouldBe` True
      isPrime' 11 `shouldBe` True

  describe "number to digits" $ do
    it "0 = [0]" $ do
      numToDigits 0 `shouldBe` [0]
    it "1 = [1]" $ do
      numToDigits 1 `shouldBe` [1]
    it "12 = [1,2]" $ do
      numToDigits 12 `shouldBe` [1,2]
    it "147 = [1,4,7]" $ do
      numToDigits 147 `shouldBe` [1,4,7]

  describe "isPalindrome" $ do
    it "[3,2,3]" $ do
      isPalindrome [3,2,3] `shouldBe` True
    it "[3,2]" $ do
      isPalindrome [3,2] `shouldBe` False

  describe "primeFactors" $ do
    it "1 is []" $ primeFactors 1 `shouldBe` []
    it "2 is [2]" $ primeFactors 2 `shouldBe` [2]
    it "3 is [3]" $ primeFactors 3 `shouldBe` [3]
    it "4 is [2,2]" $ primeFactors 4 `shouldBe` [2,2]
    it "5 is [5]" $ primeFactors 5 `shouldBe` [5]
    it "6 is [2,3]" $ primeFactors 6 `shouldBe` [2,3]
    it "8 is [2,2,2]" $ primeFactors 8 `shouldBe` [2,2,2]

    
  
  describe "Lowest Common Multiplier" $ do
    it "1, 1 = 1" $ do lowestCommonMultiplier 1 1 `shouldBe` 1
    it "1, 2 = 2" $ do lowestCommonMultiplier 1 2 `shouldBe` 2
    it "1, 3 = 3" $ do lowestCommonMultiplier 1 3 `shouldBe` 3
    it "2, 4 = 4" $ do lowestCommonMultiplier 2 4 `shouldBe` 4
    it "3, 5 = 4" $ do lowestCommonMultiplier 3 5 `shouldBe` 15
    