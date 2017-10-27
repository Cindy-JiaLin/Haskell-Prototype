module TestData.Structured.Pair where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Primitive

int1 = I 1 2 1
int2 = I 1 2 2
int3 = I 1 2 3
int4 = I 1 2 4
int5 = I 1 2 5
int1k = I 1 2 1000

str1 = S 1 1 "ABC"
str2 = S 1 1 "AB"

delta_3_5 = ω int3 int5 1

delta_3_1k = ω int3 int1k 1

{-

*TestData.Structured.Pair> delta_3_5
(Δ_Prim (3 => 5) ,0.0)
(0.04 secs, 24,869,184 bytes)

*TestData.Structured.Pair> delta_3_1k
(Δ_Prim (3 => 1000) ,0.0)
(0.00 secs, 1,031,128 bytes)

-}


-- Test Pairs

p1 = Pair int1 (Pair (Pair str1 int3) int4)
p2 = Pair int2 (Pair (Pair str2 int3) int5)

delta_p1_p1 = ω p1 p1 1
{-

*TestData.Structured.Pair> delta_p1_p1
(Id (1,(("ABC",3),4)),1.0)
(0.00 secs, 1,071,216 bytes)

-}
delta_p1_p2 = ω p1 p2 1
{-
*TestData.Structured.Pair> p1
(1,(("ABC",3),4))
*TestData.Structured.Pair> p2
(2,(("AB",3),5))

*TestData.Structured.Pair> delta_p1_p2
(Δ_Pair (Δ_Prim 1, ((ƛx.x+1), (ƛx.x-1)), 2) 
        (Δ_Pair (Δ_Pair (Δ_Prim <"ABC" => "AB"> ) 
                        (Id 3)
                ) 
                (Δ_Prim 4, ((ƛx.x+1), (ƛx.x-1)), 5)
        )
,0.7)
(0.01 secs, 1,611,448 bytes)
-}


p3 = Pair int1 (Pair (Pair str2 int3) int4)
p4 = Pair int2 (Pair (Pair str2 int3) int5)

delta_p3_p4 = ω p3 p4 1

{-

*TestData.Structured.Pair> delta_p3_p4
(Δ_Pair (Δ_Prim 1, ((ƛx.x+1), (ƛx.x-1)), 2) 
        (Δ_Pair (Id ("AB",3)) 
                (Δ_Prim 4, ((ƛx.x+1), (ƛx.x-1)), 5)
        )
,0.75)
(0.00 secs, 1,621,088 bytes)

-}



