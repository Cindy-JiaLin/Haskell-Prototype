module TestData.Structured.Others.ListOfSet where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

set1 = set 1 0 "a b c a b c d"
set2 = set 1 0 "c a a b c d a b"
set3 = set 1 0 "a b c d e f"

los1 = listT [set1, set3]
los2 = listT [set2, set3, set1]


delta_los_12 = ω los1 los2 1

{-

*TestData.Structured.Others.ListOfSet> los1
[{"d", "c", "b", "a"}, {"f", "e", "d", "c", "b", "a"}]

*TestData.Structured.Others.ListOfSet> los2
[{"b", "a", "d", "c"}, {"f", "e", "d", "c", "b", "a"}, {"d", "c", "b", "a"}]

*TestData.Structured.Others.ListOfSet> delta_los_12
(Delta_List [chg. Delta_Set [chg. Delta_Prim (="d") , 
                             chg. Delta_Prim (="c") , 
                             chg. Delta_Prim (="b") , 
                             chg. Delta_Prim (="a") 
                            ], 
             chg. Delta_Set [chg. Delta_Prim (="f") , 
                             chg. Delta_Prim (="e") , 
                             chg. Delta_Prim (="d") , 
                             chg. Delta_Prim (="c") , 
                             chg. Delta_Prim (="b") , 
                             chg. Delta_Prim (="a") 
                            ], 
            ins. {"d", "c", "b", "a"}
           ]
,0.8333333333333334)

-}
