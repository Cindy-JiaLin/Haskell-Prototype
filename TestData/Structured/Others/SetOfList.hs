module TestData.Structured.Others.SetOfList where

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

list1 = list 1 0 "a b c a b c d"
list2 = list 1 0 "c a a b c d a b"
list3 = list 1 0 "a b c d e f"

sol1 = setT [list1, list3]
sol2 = setT [list2, list3]


delta_sol_12 = ω sol1 sol2 1

{-

*TestData.Structured.Others.SetOfList> sol1
{["a", "b", "c", "d", "e", "f"], ["a", "b", "c", "a", "b", "c", "d"]}

*TestData.Structured.Others.SetOfList> sol2
{["a", "b", "c", "d", "e", "f"], ["c", "a", "a", "b", "c", "d", "a", "b"]}

*TestData.Structured.Others.SetOfList> delta_sol_12
(Delta_Set [chg. Delta_List [chg. Delta_Prim (="a") , 
                             chg. Delta_Prim (="b") , 
                             chg. Delta_Prim (="c") , 
                             chg. Delta_Prim (="d") , 
                             chg. Delta_Prim (="e") , 
                             chg. Delta_Prim (="f") 
                            ], 
            chg. Delta_List [ins. "c", 
                             chg. Delta_Prim (="a") , 
                             ins. "a", 
                             chg. Delta_Prim (="b") , 
                             chg. Delta_Prim (="c") , 
                             ins. "d", 
                             chg. Delta_Prim (="a") , 
                             chg. Delta_Prim (="b") , 
                             del. "c", 
                             del. "d"
                            ]
           ]
,0.8148148148148148)

-}

sol3 = setT [list3, list1]
sol4 = setT [list3, list2]

delta_sol_34 = ω sol3 sol4 1

{-

*TestData.Structured.Others.SetOfList> sol3
{["a", "b", "c", "a", "b", "c", "d"], ["a", "b", "c", "d", "e", "f"]}

*TestData.Structured.Others.SetOfList> sol4
{["c", "a", "a", "b", "c", "d", "a", "b"], ["a", "b", "c", "d", "e", "f"]}

*TestData.Structured.Others.SetOfList> delta_sol_34
(Delta_Set [chg. Delta_List [chg. Delta_Prim (="a") , 
                             chg. Delta_Prim (="b") , 
                             chg. Delta_Prim (="c") , 
                             del. "a", 
                             del. "b", 
                             del. "c", 
                             chg. Delta_Prim (="d") , 
                             ins. "e", 
                             ins. "f"
                            ], 
            chg. Delta_List [ins. "c", 
                             chg. Delta_Prim (="a") , 
                             ins. "a", 
                             chg. Delta_Prim (="b") , 
                             chg. Delta_Prim (="c") , 
                             chg. Delta_Prim (="d") , 
                             chg. Delta_Prim <+"a" ,-"e">, 
                             del. "f", 
                             ins. "b"
                            ]
           ]
,0.5925925925925926)

-}
