module Helper where

import Test.QuickCheck

myfib :: [Integer]
myfib = 1:2:(zipWith (+) myfib (tail myfib))

factorise :: Integer -> [Integer]
factorise x | x <= 1 = [x]
            | otherwise = f' [1] 2 x
  where f':: [Integer] -> Integer -> Integer -> [Integer]
        f' res curr currX = if curr > sqrtx
                            then currX:res
                            else if currX `mod` curr == 0
                                 then f' (curr:res) curr     (currX `div` curr)
                                 else f' res        (curr+1) currX
        sqrtx = floor (sqrt (fromIntegral x))

prop_factprod :: Integer -> Bool
prop_factprod x = x == product ( factorise x )

isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = null $ filter (\y -> x `mod` y == 0 ) [2..z]
  where z = floor (sqrt (fromIntegral x))