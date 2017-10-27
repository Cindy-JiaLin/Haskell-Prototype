module TestData.Atomic.Character where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Real Numbers

a = C 1 'a'
b = C 1 'b'
c = C 1 'c'

delta_a_b= ω a b 1

{-

*TestData.Atomic.Character> delta_a_b
(Δ_Prim ('a' => 'b') ,0.0)
(0.04 secs, 24,315,472 bytes)

-}
