module TestData.Structured.Tree where

import Data.List

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

import Main.Application.Apply
import Main.Application.Orig
import Main.Application.Targ
import Main.Application.ApplyValue

int1 = I 1 1 1
int2 = I 1 1 2
int3 = I 1 1 3
int4 = I 1 1 4
int5 = I 1 1 5
int6 = I 1 1 6
-------------------------------------------

-- Constructor of Tree 1
tree1_leaf3 = Rec (Pair int3 (List EmptyList))

tree1_leaf4 = Rec (Pair int4 (List EmptyList))
tree1_leaf5 = Rec (Pair int5 (List EmptyList))
tree1_leaf6 = Rec (Pair int6 (List EmptyList))

tree1_leaves_of_2 = listT [tree1_leaf4, tree1_leaf5, tree1_leaf6]

tree1_subtree2 = Rec (Pair int2 tree1_leaves_of_2)

tree1_branches_of_1 = listT [tree1_subtree2, tree1_leaf3]

tree1 = Rec (Pair int1 tree1_branches_of_1)

-------------------------------------------
-- Constructor of Tree 2
tree2_leaf4 = Rec (Pair int4 (List EmptyList))
tree2_leaf5 = Rec (Pair int5 (List EmptyList))
tree2_leaf6 = Rec (Pair int6 (List EmptyList))

tree2_leaves_of_2 = listT [tree2_leaf4, tree2_leaf5]
tree2_leaves_of_3 = listT [tree2_leaf6]

tree2_subtree2 = Rec (Pair int2 tree2_leaves_of_2)
tree2_subtree3 = Rec (Pair int3 tree2_leaves_of_3)

tree2_branches_of_1 = listT [tree2_subtree2, tree2_subtree3]

tree2 = Rec (Pair int1 tree2_branches_of_1)
----------------------------------------------

{-

*TestData.Structured.Tree> tree1
(1,[(2,[(4,[]), (5,[]), (6,[])]), (3,[])])

*TestData.Structured.Tree> tree2
(1,[(2,[(4,[]), (5,[])]), (3,[(6,[])])])

-}
delta_tree_12 = ω tree1 tree2 1

{-

*TestData.Structured.Tree> delta_tree_12
(Δ_Rec (Δ_Pair (Id 1) 
               (Δ_List [chg. 
                        Δ_Rec (Δ_Pair (Id 2) 
                                      (Δ_List [chg. Id (4,[]), 
                                               chg. Id (5,[]), 
                                               del. (6,[])
                                              ])
                              ), 
                        chg. 
                        Δ_Rec (Δ_Pair (Id 3) 
                                      (Δ_List [ins. (6,[])])
                              )
                       ]
               )
       )
,0.8333333333333334)
(0.05 secs, 26,375,056 bytes)

-}

tree2' = applyForward tree1 (getDelta delta_tree_12)
tree1' = applyBackward (getDelta delta_tree_12) tree2

{-

*TestData.Structured.Tree> tree2'
(1,[(2,[(4,[]), (5,[])]), (3,[(6,[])])])
(0.18 secs, 38,254,240 bytes)

*TestData.Structured.Tree> eq tree2 tree2'
True
(0.00 secs, 1,072,496 bytes)

*TestData.Structured.Tree> tree1'
(1,[(2,[(4,[]), (5,[]), (6,[])]), (3,[])])
(0.12 secs, 27,428,336 bytes)

*TestData.Structured.Tree> eq tree1 tree1'
True
(0.00 secs, 1,072,792 bytes)

-}


