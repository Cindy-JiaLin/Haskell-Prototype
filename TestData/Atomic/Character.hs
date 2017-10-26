module TestData.Atomic.Character where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

import Main.Application.Apply
------------------------------------------------------------------------------------------------
-- Test Real Numbers

a = C 1 'a'
b = C 1 'b'
c = C 1 'c'

delta_a_b= ω a b 1

af_c = applyForward c (getDelta delta_a_b)

bf_c = applyBackward (getDelta delta_a_b) c

b' = applyForward a (getDelta delta_a_b)

a' = applyBackward (getDelta delta_a_b) b

{-

*TestData.Atomic.Character> delta_a_b
(Δ_Prim ('a' => 'b') ,0.0)
(0.04 secs, 24,315,472 bytes)

*TestData.Atomic.Character> af_c
'c'
(0.00 secs, 1,030,008 bytes)

*TestData.Atomic.Character> bf_c
'c'
(0.00 secs, 516,936 bytes)

*TestData.Atomic.Character> b'
'b'
(0.00 secs, 1,073,848 bytes)

*TestData.Atomic.Character> a'
'a'
(0.00 secs, 1,073,664 bytes)

*TestData.Atomic.Character> eq a a'
True
(0.00 secs, 1,074,824 bytes)

*TestData.Atomic.Character> eq b b'
True
(0.00 secs, 1,074,608 bytes)

-}
