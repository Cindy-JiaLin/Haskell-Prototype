module TestData.Atomic.Unit where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

import Main.Application.Apply
------------------------------------------------------------------------------------------------
-- Test Unit 

delta_Unit= ω Unit Unit 1

a_f = applyForward Unit (getDelta delta_Unit)

a_b = applyBackward (getDelta delta_Unit) Unit

{-
*TestData.Atomic.Unit> delta_Unit
(Id unit,1.0)
(0.04 secs, 23,744,928 bytes)

*TestData.Atomic.Unit> a_f
unit
(0.00 secs, 518,640 bytes)

*TestData.Atomic.Unit> a_b
unit
(0.00 secs, 1,074,176 bytes)

*TestData.Atomic.Unit> eq Unit a_f
True
(0.00 secs, 1,028,320 bytes)

*TestData.Atomic.Unit> eq Unit a_b
True
(0.00 secs, 1,074,352 bytes)

-}
