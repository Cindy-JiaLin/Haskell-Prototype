module TestData.Atomic.Integer where

import Data.List

import Main.Value
import Main.Delta
import Main.Eq
import Main.Similarity
import Main.Omega

import Main.Application.Apply

------------------------------------------------------------------------------------------------
-- Test Integers

i1 = I 1 5 1
i2 = I 1 5 2
i100 = I 1 5 100

i3 = I 1 2 3

delta_i1_i2 = ω i1 i2 1
{-

*TestData.Atomic.Integer> delta_i1_i2
(Δ_Prim 1, ((ƛx.x+1), (ƛx.x-1)), 2,0.8)
(0.04 secs, 24,296,336 bytes)

-}

delta_i2_i100 = ω i2 i100 1

{-

*TestData.Atomic.Integer> delta_i2_i100
(Δ_Prim (2 => 100) ,0.0)
(0.00 secs, 1,030,832 bytes)

-}
delta_i2_i3 = ω i2 i3 1

{-

*TestData.Atomic.Integer> delta_i2_i3
(*** Exception: lwb_Delta: These two integers have different accuracy

-}

i2' = applyForward i1 (getDelta delta_i1_i2)
i1' = applyBackward (getDelta delta_i1_i2) i2

{-

*TestData.Atomic.Integer> i2'
2
(0.00 secs, 1,073,592 bytes)
*TestData.Atomic.Integer> i1'
1
(0.00 secs, 1,032,752 bytes)

-}

i100' = applyForward i2 (getDelta delta_i2_i100)
i2'' = applyBackward (getDelta delta_i2_i100) i100

{-

*TestData.Atomic.Integer> i100'
100
(0.00 secs, 1,030,200 bytes)
*TestData.Atomic.Integer> i2''
2
(0.00 secs, 517,896 bytes)

*TestData.Atomic.Integer> eq i2 i2'
True
(0.00 secs, 1,075,760 bytes)

*TestData.Atomic.Integer> eq i100 i100'
True
(0.00 secs, 1,072,736 bytes)

*TestData.Atomic.Integer> eq i2 i2''
True
(0.00 secs, 1,031,656 bytes)

*TestData.Atomic.Integer> eq i1 i1'
True
(0.00 secs, 1,075,952 bytes)
-}
