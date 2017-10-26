module TestData.Structured.Mapping where

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

import Main.Application.Apply
-----------------------------------------------------------------------------------------------
-- Test Mapping 

-- f(x) = x and f(x) = -x when x = {1, 2, 3, 4, 5}
-- identity
identity :: T -> T 
identity (I w n x) = (I w n x)
identity (R w eps x) = (R w eps x)
-- negative
neg :: T -> T 
neg (I w n x) = I w n (- x)

f :: T -> T
f (I w n x) = I w n (- x + 6)

g :: T -> T
g (R w eps x) = R w eps (x-0.3)

int1 = I 1 1 1
int2 = I 1 1 2
int3 = I 1 1 3
int4 = I 1 1 4
int5 = I 1 1 5
int6 = I 1 1 6

m1 = mapT identity [int1, int2, int3, int4, int5] 
m2 = mapT neg [int1, int2, int3, int4, int5]
m3 = mapT f [int1, int2, int3, int4, int5]

delta_m1_m3 = ω m1 m3 1

{-

*TestData.Structured.Mapping> delta_m1_m3
(Δ_Map [chg. Δ_Pair (Id 5) 
                    (Δ_Prim (5 => 1) ), 
        chg. Δ_Pair (Id 4) 
                    (Δ_Prim (4 => 2) ), 
        chg. Id (3,3), 
        chg. Δ_Pair (Id 2) 
                    (Δ_Prim (2 => 4) ), 
        chg. Δ_Pair (Id 1) 
                    (Δ_Prim (1 => 5) )
       ]
,0.6)
(0.51 secs, 110,408,312 bytes)

-}

rf1 = R 1 0.5 165.6
rf2 = R 1 0.5 208.6
rf3 = R 1 0.5 233.3
rf4 = R 1 0.5 255.3
rf5 = R 1 0.5 300.0

rg1 = R 1 0.5 82.0
rg2 = R 1 0.5 233.3
rg3 = R 1 0.5 209.0
rg4 = R 1 0.5 255.0

mapf = mapT identity [rf1, rf2, rf3, rf4, rf5]
mapg = mapT g [rg1, rg2, rg3, rg4]

delta_f_g = ω mapf mapg 1
{-

*TestData.Structured.Mapping> delta_f_g
(Δ_Map [chg. Δ_Pair (Δ_Prim 255.3, ((ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)), 255.0) 
                    (Δ_Prim (255.3 => 254.7) ), 
        chg. Δ_Pair (Id 233.3) 
                    (Δ_Prim 233.3, ((ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)), 233.0), 
        chg. Δ_Pair (Δ_Prim 208.6, ((ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)), 209.0) 
                    (Δ_Prim 208.6, ((ƛx.x+9.999999999999432e-2), (ƛx.x-9.999999999999432e-2)), 208.7), 
        chg. Δ_Pair (Δ_Prim (300.0 => 82.0) ) 
                    (Δ_Prim (300.0 => 81.7) ), 
        del. (165.6,165.6)
       ]
,0.31111111111110606)
(0.27 secs, 59,299,280 bytes)

-}

mapg' = applyForward mapf (getDelta delta_f_g)

mapf' = applyBackward (getDelta delta_f_g) mapg

{-

*TestData.Structured.Mapping> mapg'
{(82.0, 81.7), (255.0, 254.7), (233.3, 233.0), (209.0, 208.7)}
(0.16 secs, 39,760,512 bytes)

*TestData.Structured.Mapping> eq mapg mapg'
True
(0.00 secs, 1,072,848 bytes)

*TestData.Structured.Mapping> mapf'
{(255.3, 255.3), (208.6, 208.6), (233.3, 233.3), (300.0, 300.0), (165.6, 165.6)}
(0.06 secs, 15,543,008 bytes)

*TestData.Structured.Mapping> eq mapf mapf'
True
(0.00 secs, 1,029,280 bytes)

-}
