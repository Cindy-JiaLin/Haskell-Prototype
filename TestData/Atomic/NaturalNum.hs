module TestData.Atomic.NaturalNum where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

import Main.Application.Apply
------------------------------------------------------------------------------------------------
-- Test Real Numbers

n1 = Nat 1 2
n2 = Nat 1 100
n3 = Nat 1 35

delta_n1_n2= ω n1 n2 1

af_n3 = applyForward n3 (getDelta delta_n1_n2)

bf_n3 = applyBackward (getDelta delta_n1_n2) n3

n2' = applyForward n1 (getDelta delta_n1_n2)

n1' = applyBackward (getDelta delta_n1_n2) n2

{-

*TestData.Atomic.NaturalNum> delta_n1_n2
(Δ_Prim (2 => 100) ,0.0)
(0.04 secs, 24,301,600 bytes)

*TestData.Atomic.NaturalNum> af_n3
35
(0.00 secs, 1,031,264 bytes)

*TestData.Atomic.NaturalNum> bf_n3
35
(0.00 secs, 1,031,344 bytes)

*TestData.Atomic.NaturalNum> n2'
100
(0.00 secs, 1,032,376 bytes)

*TestData.Atomic.NaturalNum> n1'
2
(0.00 secs, 1,074,640 bytes)

*TestData.Atomic.NaturalNum> eq n2 n2'
True
(0.00 secs, 1,076,200 bytes)

*TestData.Atomic.NaturalNum> eq n1 n1'
True
(0.00 secs, 1,076,176 bytes)

-}
