module Q1to10Benchmark where

import Criterion.Main (defaultMain, bench, whnf, nf)
import Q1to10
import EulerLib


q1to10Benchmarks :: IO ()
q1to10Benchmarks = defaultMain (
    q1Bench <>
    q2Bench <>
    q3Bench <>
    q4Bench <>
    q5Bench 
    -- q7Bench  -- slow!
  )

q1Bench = [bench "Q1" $ whnf sumOfThings 1000]

q2Bench = [bench "Q2" $ whnf sumOfFibUnder 4000000]

q3input = 600851475143
q3Bench = 
  [ bench "Q3: first attempt" $ whnf maxPrimeFactor q3input
  , bench "Q3: optimised" $ whnf maxPrimeFactorOptimised q3input 
  ]

q4Input = 999
q4Bench = 
  [ bench "Q4: v1" $ whnf biggestPalindromeProductOfV1 q4Input
  , bench "Q4: v2" $ whnf biggestPalindromeProductOfV2 q4Input 
  , bench "Q4: v3" $ whnf biggestPalindromeProductOfV3 q4Input 
  , bench "Q4: v4" $ whnf biggestPalindromeProductOfV4 q4Input 
  ]

q5Bench = 
  [ bench "Q5" $ whnf smallestNumberThatCanBeDividedBy  [1..20]
  , bench "Q5 LCM from right 20-1" $ whnf q5r (enumFromThenTo 20 19 1)
  , bench "Q5 LCM from left 20-1"  $ whnf q5l (enumFromThenTo 20 19 1)
  , bench "Q5 LCM from right 1-20" $ whnf q5r (enumFromThenTo 1 2 20)
  , bench "Q5 LCM from left 1-20"  $ whnf q5l (enumFromThenTo 1 2 20)
  ]

q7Bench = 
    [ bench "1th prime" $ nf nthPrime (1 :: Integer)
    , bench "10,001th prime" $ nf nthPrime (10001 :: Integer)
    , bench "1th nthPrimeSeive" $ nf nthPrimeSeive (1 :: Integer)
    , bench "10,001th nthPrimeSeive" $ nf nthPrimeSeive (10001 :: Integer)
    ]