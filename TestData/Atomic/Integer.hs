module TestData.Atomic.Integer where

import Data.List

import Main.Value
import Main.Delta
import Main.Eq
import Main.Similarity
import Main.Omega

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

