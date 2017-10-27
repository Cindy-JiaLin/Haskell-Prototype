module TestData.Atomic.NaturalNum where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Real Numbers

n1 = Nat 1 2
n2 = Nat 1 100
n3 = Nat 1 35

delta_n1_n2= ω n1 n2 1


{-

*TestData.Atomic.NaturalNum> delta_n1_n2
(Δ_Prim (2 => 100) ,0.0)
(0.04 secs, 24,301,600 bytes)

-}
