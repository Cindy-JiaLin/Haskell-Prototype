module TestData.Atomic.String where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Omega

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

