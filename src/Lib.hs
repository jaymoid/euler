module Lib where
    

eqDiv :: Integral a => a -> a -> Bool
eqDiv dividend divisior = dividend `mod` divisior == 0

fib = 1 : scanl (+) 1 fib