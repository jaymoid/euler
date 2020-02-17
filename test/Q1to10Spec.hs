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
    it "answer" $ do
      maxPrimeFactor 600851475143 `shouldBe` Just 6857
    it "sample mpf function" $ 
      mpfExplained 13195 `shouldBe`  29
    it "answer mpf function" $ do
      mpfExplained 600851475143 `shouldBe`  6857


  describe "Q4" $ do
    it "Sample" $ 
      biggestPalindromeProductOfV1 99 `shouldBe` (99, 91, 9009)
    it "answer" $ 
      biggestPalindromeProductOfV1 999 `shouldBe` (993, 913, 906609)

  describe "Q5" $ do
    it "Sample" $ 
      smallestNumberThatCanBeDividedBy [1..10] `shouldBe` 2520
    it "answer" $ 
      smallestNumberThatCanBeDividedBy [1..20] `shouldBe` 232792560

  describe "Q6" $ do
    it "Sample: Sum of squares of first ten numbers = 385" $ 
      sumOfSquaresOf [1..10] `shouldBe` 385
    it "Sample: The square of the sum of the first ten natural numbers is = 3025" $
      squareOfTheSumOf [1..10] `shouldBe` 3025
    it "Sample: Difference between sum of the squares of the first ten natural \
       \ numbers and the square of the sum is= 3025−385 = 2640" $
      diffSumOfSquaresAndSquareOfSums [1..10] `shouldBe` 2640 
    it "Answer: Find the difference between the sum of the squares of the first\
       \ one hundred natural numbers and the square of the sum. " $ 
      diffSumOfSquaresAndSquareOfSums [1..100] `shouldBe` 25164150
  
  describe "Q7 prime spike" $ do
    it "Sample: The first 6 primes are [2, 3, 5, 7, 11, 13]" $ do
      nthPrime 1 `shouldBe` 2
      nthPrime 2 `shouldBe` 3
      nthPrime 3 `shouldBe` 5
      nthPrime 4 `shouldBe` 7
      nthPrime 5 `shouldBe` 11
      nthPrime 6 `shouldBe` 13
      
  describe "Q7" $ do
    -- SLOW nthPrime
    -- it "Answer: the 10,001th prime is 104743" $
    --   nthPrime 10001 `shouldBe` 104743
    -- FASTER!
    it "Answer: the 10,001th prime is 104743" $
      nthPrimeSeive 10001 `shouldBe` 104743

  let q8Input = "73167176531330624919225119674426574742355349194934\
    \96983520312774506326239578318016984801869478851843\
    \85861560789112949495459501737958331952853208805511\
    \12540698747158523863050715693290963295227443043557\
    \66896648950445244523161731856403098711121722383113\
    \62229893423380308135336276614282806444486645238749\
    \30358907296290491560440772390713810515859307960866\
    \70172427121883998797908792274921901699720888093776\
    \65727333001053367881220235421809751254540594752243\
    \52584907711670556013604839586446706324415722155397\
    \53697817977846174064955149290862569321978468622482\
    \83972241375657056057490261407972968652414535100474\
    \82166370484403199890008895243450658541227588666881\
    \16427171479924442928230863465674813919123162824586\
    \17866458359124566529476545682848912883142607690042\
    \24219022671055626321111109370544217506941658960408\
    \07198403850962455444362981230987879927244284909188\
    \84580156166097919133875499200524063689912560717606\
    \05886116467109405077541002256983155200055935729725\
    \71636269561882670428252483600823257530420752963450"

  describe "Q8" $ do
    it "max 4 adjacent digit product: 9 × 9 × 8 × 9 = 5832." $ 
      maxAdjacentProduct 4 q8Input `shouldBe` 5832
    
    it "max 13 adjacent digit product = 23514624000." $ 
      maxAdjacentProduct 13 q8Input `shouldBe` 23514624000


  describe "Q9" $ do 
    it "Sample: Pythagorean triple where the sum of the sides == 25" $ do
      pythagoreanTriples 12 `shouldBe` (3,4,5)
    it "Answer: Pythagorean triple where the sum of the sides == 25" $ do
      pythagoreanTriples 1000 `shouldBe` (200,375,425)
    it "Q9 answer" $ do
      q9 1000 `shouldBe` 31875000
