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

