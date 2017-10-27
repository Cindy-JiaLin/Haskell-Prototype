module TestData.Structured.List where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Lists

list1 = list 1 0 "a b c a b c d"
list2 = list 1 0 "c a a b c d a b"

delta_list1_list1 = ω list1 list1 1
{-

*TestData.Structured.List> delta_list1_list1
(Id ["a", "b", "c", "a", "b", "c", "d"],1.0)
(0.06 secs, 27,958,800 bytes)

-}
delta_list1_list2 = ω list1 list2 1
 
{-

*TestData.Structured.List> delta_list1_list2
(Δ_List [ins. "c", 
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
,0.6666666666666666)
(0.04 secs, 10,387,240 bytes)

-}

-------------------------------------------------------------------------
listA = lC 1 ['a', 'b', 'c', 'a', 'b', 'c', 'd']
listB = lC 1 ['c', 'a', 'a', 'b', 'c', 'd', 'a', 'b']

delta_listA_listB = ω listA listB 1
{-

*TestData.Structured.List> listA
['a', 'b', 'c', 'a', 'b', 'c', 'd']
(0.00 secs, 1,031,376 bytes)

*TestData.Structured.List> listB
['c', 'a', 'a', 'b', 'c', 'd', 'a', 'b']
(0.00 secs, 1,031,112 bytes)

*TestData.Structured.List> delta_listA_listB
(Δ_List [ins. 'c', 
         chg. Id 'a', 
         ins. 'a', 
         chg. Id 'b', 
         chg. Id 'c', 
         ins. 'd', 
         chg. Id 'a', 
         chg. Id 'b', 
         del. 'c', 
         del. 'd'
        ]
,0.6666666666666666)
(0.05 secs, 9,835,072 bytes)

-}

--------------------------------------------------------------------------
-- Test list of pairs
int1 = I 1 1 1
int2 = I 1 1 2
int3 = I 1 1 3
int4 = I 1 1 4
int5 = I 1 1 5

p11 = Pair int1 int1
p22 = Pair int2 int2
p33 = Pair int3 int3
p44 = Pair int4 int4
p55 = Pair int5 int5

p12 = Pair int1 int2
p23 = Pair int2 int3
p34 = Pair int3 int4
p45 = Pair int4 int5

p54 = Pair int5 int4
p43 = Pair int4 int3
p32 = Pair int3 int2
p21 = Pair int2 int1

lp1 = listT [p11, p22, p33, p44, p55] 
lp2 = listT [p11, p22, p32, p45, p55]

delta_lp1_lp2 = ω lp1 lp2 1
{-

*TestData.Structured.List> delta_lp1_lp2
(Δ_List [chg. Id (1,1), 
         chg. Id (2,2), 
         chg. Δ_Pair (Id 3) 
                     (Δ_Prim (3 => 2) ), 
         chg. Δ_Pair (Id 4) 
                     (Δ_Prim (4 => 5) ), 
         chg. Id (5,5)
        ]
,0.8)
(0.02 secs, 4,675,384 bytes)

-}



