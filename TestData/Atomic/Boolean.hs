module TestData.Atomic.Boolean where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Real Numbers

t = B 1 True
f = B 1 False

delta_tt = ω t t 1
delta_tf = ω t f 1
delta_ft = ω f t 1
delta_ff = ω f f 1

{-

*TestData.Atomic.Boolean> delta_tt
(Id True,1.0)
(0.03 secs, 24,279,208 bytes)

*TestData.Atomic.Boolean> delta_tf
(Δ_Prim (True => False) ,0.0)
(0.00 secs, 1,027,152 bytes)

*TestData.Atomic.Boolean> delta_ft
(Δ_Prim (False => True) ,0.0)
(0.00 secs, 1,025,616 bytes)

*TestData.Atomic.Boolean> delta_ff
(Id False,1.0)
(0.00 secs, 1,027,472 bytes)

-}
