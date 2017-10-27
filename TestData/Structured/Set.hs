module TestData.Structured.Set where

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
--Test Data
------------------------------------------------------------------------------------------------

set1 = set 1 0 "a b c a b c d e"
set2 = set 1 0 "c a a b c d a b"

delta_set1_set2 = ω set1 set2 1

{-

*TestData.Structured.Set> set1
{"e", "d", "c", "b", "a"}

*TestData.Structured.Set> set2
{"b", "a", "d", "c"}

*TestData.Structured.Set> delta_set1_set2
(Δ_Set [chg. Id "d", 
        chg. Id "c", 
        chg. Id "b", 
        chg. Id "a", 
        del. "e"
       ]
,0.8888888888888888)
(0.07 secs, 16,013,216 bytes)

-}


setA = sR 1 0.5 [165.6, 208.6, 233.3, 255.3, 300]
setB = sR 1 0.5 [82.0, 233.3, 209.0, 255.0]


delta_setA_setB = ω setA setB 1

{-

*TestData.Structured.Set> delta_setA_setB
(Δ_Set [chg. Δ_Prim 255.3, ((ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)), 255.0, 
        chg. Id 233.3, 
        chg. Δ_Prim 208.6, ((ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)), 209.0, 
        chg. Δ_Prim (300.0 => 82.0) , 
        del. 165.6
       ]
,0.35555555555554796)
(0.07 secs, 16,546,440 bytes)

-}

