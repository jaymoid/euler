
import Q1to10Benchmark
import Q1to10
import Data.Time

main :: IO () 
main = do
  start <- getCurrentTime
  -- let i = nthPrimeSeive 10001 
  -- print i
  let x = smallestNumberThatCanBeDividedByNaive  [1..20]
  print x
  stop <- getCurrentTime
  print $ diffUTCTime stop start
  return ()