module Main.Similarity
( sim_I
, sim_R
, sim_S
) where

import Main.Value

sim_R :: Double -> Double -> Double -> Double
sim_R x y ε = if (abs (x-y)) < ε 
              then 1 - (abs ((x-y)/ε))
              else 0

sim_I :: Int -> Int -> Int -> Double 
sim_I x y n = if (abs (x-y)) < n
              then 1 - (abs ((fromIntegral (x-y))/(fromIntegral n)))
              else 0


sim_S :: String -> String -> Int -> Double
sim_S "" "" _ = 1
sim_S x "" _ = 0
sim_S "" y _ = 0
sim_S x y n = let lx = length x
                  ly = length y
               in if (abs (lx-ly)) > n then 0
                  else if (count x y) > n then 0
                       else 1 - (abs ((fromIntegral (count x y))/(fromIntegral (lx +ly))))


-- This count function is used to 
-- compare characters of the same index number in two strings
-- returns number of different character pairs

count :: String -> String -> Int
count "" "" = 0
count x "" = length x
count "" y = length y
count x y = if (head x) == (head y) then count (tail x) (tail y) 
            else 2+(count (tail x) (tail y))
