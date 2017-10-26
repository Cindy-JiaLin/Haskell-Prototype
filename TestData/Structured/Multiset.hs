module TestData.Structured.Multiset where

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

msetA = mR 1 0.5 [165.6, 208.6, 233.3, 255.3, 300.0]
msetB = mR 1 0.5 [209.0, 82.0, 233.3, 209.0, 255.0, 255.0]

delta_msetA_msetB = ω msetA msetB 1
{-

*TestData.Structured.Multiset> msetA
<300.0, 255.3, 233.3, 208.6, 165.6>
(0.04 secs, 24,272,216 bytes)

*TestData.Structured.Multiset> msetB
<255.0, 255.0, 209.0, 209.0, 233.3, 82.0>
(0.00 secs, 1,029,944 bytes)

*TestData.Structured.Multiset> delta_msetA_msetB
(Δ_MSet [chg. Δ_Prim 255.3, ((ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)), 255.0, 
         chg. Id 233.3, 
         chg. Δ_Prim 208.6, ((ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)), 209.0, 
         chg. Δ_Prim (300.0 => 255.0) , 
         chg. Δ_Prim (165.6 => 209.0) , 
         ins. 82.0
        ]
,0.29090909090908473)
(0.31 secs, 73,826,664 bytes)

-}

msetB' = applyForward msetA (getDelta delta_msetA_msetB)

msetA' = applyBackward (getDelta delta_msetA_msetB) msetB

{-

*TestData.Structured.Multiset> msetB'
<209.0, 209.0, 233.3, 255.0, 255.0, 82.0>
(0.05 secs, 13,975,896 bytes)

*TestData.Structured.Multiset> eq msetB msetB'
True
(0.00 secs, 1,621,520 bytes)

*TestData.Structured.Multiset> msetA'
<233.3, 208.6, 255.3, 165.6, 300.0>
(0.12 secs, 30,527,720 bytes)

*TestData.Structured.Multiset> eq msetA msetA'
True
(0.00 secs, 1,620,984 bytes)

-}

-----------------------------------------------------------------------------------------------

msetC = mR 1 0.5 [166.0, 166.0, 209.0, 209.0, 233.0, 255.0, 300.4]


next_msetC = applyForward msetC (getDelta delta_msetA_msetB)

prev_msetC = applyBackward (getDelta delta_msetA_msetB) msetC


{-

*TestData.Structured.Multiset> msetC
<300.4, 255.0, 233.0, 209.0, 209.0, 166.0, 166.0>
(0.00 secs, 1,031,376 bytes)

*TestData.Structured.Multiset> next_msetC
<209.0, 209.0, 166.0, 209.4, 233.0, 254.7, 82.0, 255.0>
(0.13 secs, 33,542,632 bytes)

*TestData.Structured.Multiset> prev_msetC
<166.0, 166.0, 300.4, 233.0, 208.6, 255.3, 165.6, 300.0>
(0.29 secs, 71,236,384 bytes)

-}
