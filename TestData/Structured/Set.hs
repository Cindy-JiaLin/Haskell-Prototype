module TestData.Structured.Set where

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

import Main.Application.Orig
import Main.Application.Targ

import Main.Application.ApplyValue
import Main.Application.ApplyDelta
import Main.Application.ApplySolution
import Main.Application.Apply
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

set2'= applyForward set1 (getDelta delta_set1_set2)
set1'= applyBackward (getDelta delta_set1_set2) set2


{-

*TestData.Structured.Set> set2'
{"d", "c", "b", "a"}
(0.06 secs, 15,027,376 bytes)
*TestData.Structured.Set> eq set2 set2'
True
(0.00 secs, 1,072,768 bytes)

*TestData.Structured.Set> set1'
{"b", "a", "d", "c", "e"}
(0.02 secs, 5,703,768 bytes)
*TestData.Structured.Set> eq set1 set1'
True
(0.00 secs, 1,072,816 bytes)

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


setB' = applyForward setA (getDelta delta_setA_setB)

setA' = applyBackward (getDelta delta_setA_setB) setB

{-

*TestData.Structured.Set> setB'
{255.0, 233.3, 209.0, 82.0}
(0.05 secs, 14,008,920 bytes)

*TestData.Structured.Set> eq setB setB'
True
(0.00 secs, 1,071,304 bytes)

*TestData.Structured.Set> setA'
{255.3, 208.6, 233.3, 300.0, 165.6}
(0.02 secs, 5,739,792 bytes)

*TestData.Structured.Set> eq setA setA'
True
(0.00 secs, 1,071,056 bytes)

-}

setC = sR 1 0.5 [166, 209, 233, 255, 300.4, 77]

setC_target = applyForward setC (getDelta delta_setA_setB)

{-

*TestData.Structured.Set> setC
{77.0, 300.4, 255.0, 233.0, 209.0, 166.0}
(0.00 secs, 1,030,816 bytes)

*TestData.Structured.Set> setC_target
{254.7, 233.0, 209.4, 77.0, 82.0}
(0.09 secs, 23,236,592 bytes)

-}
