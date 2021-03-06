module TestData.Structured.Union where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------

list1 = list 1 0 "a b c a b c d"
list2 = list 1 0 "c a a b c d a b"


u1 = Union (Lt list1)
u2 = Union (Lt list2)
u3 = Union (Rt list1)
u4 = Union (Rt list2)

delta_u1_u1 = ω u1 u1 1
{-

*TestData.Structured.Union> delta_u1_u1
(Id Left ["a", "b", "c", "a", "b", "c", "d"]
,1.0)
(0.02 secs, 4,155,464 bytes)

-}
delta_u1_u2 = ω u1 u2 1

{-

*TestData.Structured.Union> delta_u1_u2
(Δ_Union ll.( Δ_List [ins. "c", 
                      chg. Id "a", 
                      ins. "a", 
                      chg. Id "b", 
                      chg. Id "c", 
                      ins. "d", 
                      chg. Id "a", 
                      chg. Id "b", 
                      del. "c", 
                      del. "d"
                     ] 
            )
,0.6666666666666666)
(0.09 secs, 33,635,760 bytes)

-}
delta_u1_u3 = ω u1 u3 1
{-
*TestData.Structured.Union> delta_u1_u3
(Δ_Union lr.( ["a", "b", "c", "a", "b", "c", "d"], ["a", "b", "c", "a", "b", "c", "d"] )
,0.0)
(0.00 secs, 1,071,344 bytes)

-}
delta_u3_u2 = ω u3 u2 1
{-

*TestData.Structured.Union> delta_u3_u2
(Δ_Union rl.( ["a", "b", "c", "a", "b", "c", "d"], ["c", "a", "a", "b", "c", "d", "a", "b"] )
,0.0)
(0.00 secs, 1,025,832 bytes)

-}
delta_u3_u4 = ω u3 u4 1
{-

*TestData.Structured.Union> delta_u3_u4
(Δ_Union rr.( Δ_List [ins. "c", 
                      chg. Id "a", 
                      ins. "a", 
                      chg. Id "b", 
                      chg. Id "c", 
                      ins. "d", 
                      chg. Id "a", 
                      chg. Id "b", 
                      del. "c", 
                      del. "d"
                     ] 
            )
,0.6666666666666666)
(0.04 secs, 10,394,592 bytes)

-}

