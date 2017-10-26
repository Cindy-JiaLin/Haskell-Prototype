module TestData.Atomic.String where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Omega

import Main.Application.Apply
------------------------------------------------------------------------------------------------
-- Test Strings

s1 = S 1 3 "color"
s2 = S 1 3 "colour"

s3 = S 1 3 "collect"

delta_s1_s2 = ω s1 s2 1
{-

*TestData.Atomic.String> delta_s1_s2
(Δ_Prim <"color" => "colour"> ,0.7272727272727273)
(0.04 secs, 24,853,304 bytes)

-}

delta_s1_s3 = ω s1 s3 1
{-

*TestData.Atomic.String> delta_s1_s3
(Δ_Prim <"color" => "collect"> ,0.0)
(0.00 secs, 1,029,472 bytes)

-}

s2' = applyForward s1 (getDelta delta_s1_s2)
s1' = applyBackward (getDelta delta_s1_s2) s2

s3' = applyForward s1 (getDelta delta_s1_s3)

s1'' = applyBackward (getDelta delta_s1_s3) s3

{-
*TestData.Atomic.String> s2'
"colour"
(0.00 secs, 1,074,264 bytes)

*TestData.Atomic.String> s1'
"color"
(0.00 secs, 1,032,768 bytes)

*TestData.Atomic.String> s3'
"collect"
(0.00 secs, 1,032,448 bytes)

*TestData.Atomic.String> s1''
"color"
(0.00 secs, 1,030,688 bytes)

*TestData.Atomic.String> eq s1 s1'
True
(0.00 secs, 1,075,920 bytes)

*TestData.Atomic.String> eq s1 s1''
True
(0.00 secs, 1,033,440 bytes)

*TestData.Atomic.String> eq s2 s2'
True
(0.00 secs, 1,075,736 bytes)

*TestData.Atomic.String> eq s3 s3'
True
(0.00 secs, 1,033,144 bytes)

-}
