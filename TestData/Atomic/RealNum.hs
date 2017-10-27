module TestData.Atomic.RealNum where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Real Numbers

r2086_1 = R 1 0.1 208.6
r209_1 = R 1 0.1 209

r2086_5 = R 1 0.5 208.6
r209_5 = R 1 0.5 209

delta_r2086_r209_1 = ω r2086_1 r209_1 1

delta_r2086_r209_5 = ω r2086_5 r209_5 1 
 
{-

*TestData.Atomic.RealNum> delta_r2086_r209_1
(Δ_Prim (208.6 => 209.0) ,0.0)
(0.03 secs, 24,271,304 bytes)

*TestData.Atomic.RealNum> delta_r2086_r209_5
(Δ_Prim 208.6, ((ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)), 209.0
,0.19999999999998863)
(0.00 secs, 1,032,696 bytes)

-}



r1 = R 1 0.01 1
r2 = R 1 0.01 2
r3 = R 1 0.01 3
r0796 = R 1 0.01 0.796
r08 = R 1 0.01 0.8

delta_r1_r2 = ω r1 r2 1 

{-

*TestData.Atomic.RealNum> delta_r1_r2
(Δ_Prim (1.0 => 2.0) ,0.0)
(0.00 secs, 1,027,936 bytes)

-}

delta_r0796_r08 = ω r0796 r08 1 
{-

*TestData.Atomic.RealNum> delta_r0796_r08
(Δ_Prim 0.796, ((ƛx.x+4.0000000000000036e-3), (ƛx.x-4.0000000000000036e-3)), 0.8
,0.5999999999999996)
(0.00 secs, 1,030,968 bytes)

-}
