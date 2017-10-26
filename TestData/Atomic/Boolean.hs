module TestData.Atomic.Boolean where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

import Main.Application.Apply
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

applyForward_t_tt = applyForward t (getDelta delta_tt)
applyForward_t_tf = applyForward t (getDelta delta_tf)
applyForward_t_ft = applyForward t (getDelta delta_ft)
applyForward_t_ff = applyForward t (getDelta delta_ff)
{-

*TestData.Atomic.Boolean> applyForward_t_tt
True
(0.00 secs, 1,030,672 bytes)

*TestData.Atomic.Boolean> applyForward_t_tf
False
(0.00 secs, 1,032,768 bytes)

*TestData.Atomic.Boolean> applyForward_t_ft
True
(0.00 secs, 1,033,360 bytes)

*TestData.Atomic.Boolean> applyForward_t_ff
True
(0.00 secs, 517,008 bytes)

-}

applyForward_f_tt = applyForward f (getDelta delta_tt)
applyForward_f_tf = applyForward f (getDelta delta_tf)
applyForward_f_ft = applyForward f (getDelta delta_ft)
applyForward_f_ff = applyForward f (getDelta delta_ff)

{-

*TestData.Atomic.Boolean> applyForward_f_tt
False
(0.00 secs, 519,776 bytes)

*TestData.Atomic.Boolean> applyForward_f_tf
False
(0.00 secs, 1,033,512 bytes)

*TestData.Atomic.Boolean> applyForward_f_ft
True
(0.00 secs, 1,033,544 bytes)

*TestData.Atomic.Boolean> applyForward_f_ff
False
(0.00 secs, 1,073,816 bytes)

-}

applyBackward_t_tt = applyBackward (getDelta delta_tt) t
applyBackward_t_tf = applyBackward (getDelta delta_tf) t
applyBackward_t_ft = applyBackward (getDelta delta_ft) t
applyBackward_t_ff = applyBackward (getDelta delta_ff) t
{-

*TestData.Atomic.Boolean> applyBackward_t_tt
True
(0.00 secs, 1,034,360 bytes)

*TestData.Atomic.Boolean> applyBackward_t_tf
True
(0.00 secs, 518,240 bytes)

*TestData.Atomic.Boolean> applyBackward_t_ft
False
(0.00 secs, 1,034,192 bytes)

*TestData.Atomic.Boolean> applyBackward_t_ff
True
(0.00 secs, 1,033,992 bytes)

-}

applyBackward_f_tt = applyBackward (getDelta delta_tt) f
applyBackward_f_tf = applyBackward (getDelta delta_tf) f
applyBackward_f_ft = applyBackward (getDelta delta_ft) f
applyBackward_f_ff = applyBackward (getDelta delta_ff) f
{-

*TestData.Atomic.Boolean> applyBackward_f_tt
False
(0.00 secs, 518,112 bytes)

*TestData.Atomic.Boolean> applyBackward_f_tf
True
(0.00 secs, 518,736 bytes)

*TestData.Atomic.Boolean> applyBackward_f_ft
False
(0.00 secs, 1,032,056 bytes)

*TestData.Atomic.Boolean> applyBackward_f_ff
False
(0.00 secs, 1,034,288 bytes)

-}
