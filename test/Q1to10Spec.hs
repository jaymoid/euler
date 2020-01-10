module Q1to10Spec where

import Test.Hspec

import Q1to10

testQ1to10 :: IO ()
testQ1to10 = hspec $ do

  describe "Q1" $ do
    it "example" $ do
      sumOfThings 10 `shouldBe` 23
    it "answer" $ do
      sumOfThings 1000 `shouldBe` 233168

  describe "Q2" $ do
    it "answer" $ do
      sumOfFibUnder 4000000 `shouldBe` 4613732

  describe "Q3" $ do
    it "tdding it!" $ do
      maxPrimeFactor 0 `shouldBe` Nothing
      maxPrimeFactor 1 `shouldBe` Nothing
      maxPrimeFactor 2 `shouldBe` Just 2
      maxPrimeFactor 3 `shouldBe` Just 3
      maxPrimeFactor 4 `shouldBe` Just 2
      maxPrimeFactor 5 `shouldBe` Just 5
      maxPrimeFactor 6 `shouldBe` Just 3
      maxPrimeFactor 7 `shouldBe` Just 7
      maxPrimeFactor 8 `shouldBe` Just 2
    it "sample" $ 
      maxPrimeFactor 13195 `shouldBe` Just 29
    it "answer" $ 
      maxPrimeFactor 600851475143 `shouldBe` Just 6857
      
      
      