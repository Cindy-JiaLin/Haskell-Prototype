module TestData.Structured.Multiset where

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

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

