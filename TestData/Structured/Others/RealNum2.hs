module TestData.Structured.Others.RealNum2 where

import Data.List

import Main.Value
import Main.Delta
import Main.Eq
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Real Numbers

r1 = R 1 0.01 1
r2 = R 1 0.01 2
r3 = R 1 0.01 3
r0796 = R 1 0.01 0.796
r08 = R 1 0.01 0.8

delta_r1_r2 = ω r1 r2 1

{-

*TestData.Structured.Others.RealNum2> delta_r1_r2
(Delta_Prim (-1.0, +2.0) ,0.0)

-}
delta_r0796_r08 = ω r0796 r08 1
{-

*TestData.Structured.Others.RealNum2> delta_r0796_r08
(Delta_Prim 0.796, ((ƛx.x+4.0000000000000036e-3), (ƛx.x-4.0000000000000036e-3)), 0.8
,0.5999999999999996)
-}
----------------------------------------------------------------------------------------
-- Test list of Real Numbers
lR1 = lR 1 0.01 [1, 2, 0.8]
lR2 = lR 1 0.01 [2, 0.796, 1, 3]

delta_lR_12 = ω lR1 lR2 1
{-

*TestData.Structured.Others.RealNum2> delta_lR_12
(Delta_List [del. 1.0, 
             chg. Delta_Prim (=2.0) , 
             chg. Delta_Prim 0.8, ((ƛx.x-4.0000000000000036e-3), (ƛx.x+4.0000000000000036e-3)), 0.796, 
             ins. 1.0, 
             ins. 3.0
            ]
,0.457142857142857)
-}


-----------------------------------------------------------------------------------------
-- Test set of Real Numbers
sR1 = sR 1 0.01 [1, 2, 0.8]
sR2 = sR 1 0.01 [2, 0.796, 1, 3]

delta_sR_12 = ω sR1 sR2 1
{-

*TestData.Structured.Others.RealNum2> delta_sR_12
(Delta_Set [chg. Delta_Prim 0.8, ((ƛx.x-4.0000000000000036e-3), (ƛx.x+4.0000000000000036e-3)), 0.796, 
            chg. Delta_Prim (=2.0) , 
            chg. Delta_Prim (=1.0) , 
            ins. 3.0
           ]
,0.7428571428571428)
-}
-----------------------------------------------------------------------------------------
l1 = lR 1 0.05 [1]
l2 = lR 1 0.05 [2]
l3 = lR 1 0.05 [3]
l12 = lR 1 0.05 [1, 2]
l123 = lR 1 0.05 [1, 2, 3]
l31 = lR 1 0.05 [3.1]
l078 = lR 1 0.05 [0, 0.78, 0.9]
l081 = lR 1 0.05 [0, 0.81, 1, 1.1]


delta_lR_11 = ω l1 l1 1
{-

*TestData.Structured.Others.RealNum2> delta_lR_11
(Delta_List [chg. Delta_Prim (=1.0) ],1.0)

-}



delta_lR_078081 = ω l078 l081 1

{-

*TestData.Structured.Others.RealNum2> delta_lR_078081
(Delta_List [chg. Delta_Prim (=0.0) , 
             chg. Delta_Prim 0.78, ((ƛx.x+3.0000000000000027e-2), (ƛx.x-3.0000000000000027e-2)), 0.81, 
             del. 0.9, 
             ins. 1.0, 
             ins. 1.1
            ]
,0.39999999999999986)

-}

llR1 = listT [l1, l2, l081, l31]
llR2 = listT [l2, l078, l3]

delta_llR_12 = ω llR1 llR2 1

{-

*TestData.Structured.Others.RealNum2> delta_llR_12
(Delta_List [del. [1.0], 
             chg. Delta_List [chg. Delta_Prim (=2.0) ], 
             chg. Delta_List [chg. Delta_Prim (=0.0) , 
                              chg. Delta_Prim 0.81, ((ƛx.x-3.0000000000000027e-2), (ƛx.x+3.0000000000000027e-2)), 0.78, 
                              del. 1.0, 
                              del. 1.1, 
                              ins. 0.9
                             ], 
             del. [3.1], 
             ins. [3.0]
            ]
,0.3999999999999999)

-}


llR3 = listT [l2]

delta_llR_33 = ω llR3 llR3 1
{-

*TestData.Structured.Others.RealNum2> delta_llR_33
(Delta_List [chg. Delta_List [chg. Delta_Prim (=2.0) ]],1.0)
-}



llR4 = listT [l123]

delta_llR_44 = ω llR4 llR4 1
{-

*TestData.Structured.Others.RealNum2> delta_llR_44
(Delta_List [chg. Delta_List [chg. Delta_Prim (=1.0) , 
                              chg. Delta_Prim (=2.0) , 
                              chg. Delta_Prim (=3.0) 
                             ]
            ]
,1.0)

-}


