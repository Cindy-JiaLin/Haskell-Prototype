module Main.Eq
(eqDouble
) where

eqDouble :: Double -> Double -> Bool
eqDouble x y = if (abs (x-y)) < 1.0e-6 then True
               else False
