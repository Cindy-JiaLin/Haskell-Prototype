module TestData.Structured.Others.Integer2 where

import Data.List

import Main.Value
import Main.Delta
import Main.Eq
import Main.Similarity
import Main.Solution
import Main.Omega

import Main.Application.Apply
------------------------------------------------------------------------------------------------
-- Test Real Numbers

lInt1 = lI 1 2 [3, 9, 100, 101]
lInt2 = lI 1 2 [7, 3, 9, 100, 101]

delta_lInt_12 = ω lInt1 lInt2 1

lInt2' = applyForward lInt1 (getDelta delta_lInt_12)
lInt1' = applyBackward (getDelta delta_lInt_12) lInt2
{-

*TestData.Structured.Others.Integer2> lInt1
[3, 9, 100, 101]
*TestData.Structured.Others.Integer2> lInt2
[7, 3, 9, 100, 101]

*TestData.Structured.Others.Integer2> delta_lInt_12
(Delta_List [ins. 7, 
             chg. Delta_Prim (=3) , 
             chg. Delta_Prim (=9) , 
             chg. Delta_Prim (=100) , 
             chg. Delta_Prim (=101) 
            ]
,0.8888888888888888)

*TestData.Structured.Others.Integer2> lInt2'
[7, 3, 9, 100, 101]
*TestData.Structured.Others.Integer2> eq lInt2 lInt2'
True

*TestData.Structured.Others.Integer2> lInt1'
[3, 9, 100, 101]
*TestData.Structured.Others.Integer2> eq lInt1 lInt1'
True

-}

delta_lInt_12_half = ω lInt1 lInt2 0.5
{-

*TestData.Structured.Others.Integer2> delta_lInt_12_half
(Delta_List [ins. 7, 
             chg. Delta_Prim (=3) , 
             chg. Delta_Prim (=9) , 
             chg. Delta_Prim (=100) , 
             unknownRest
            ]
,0.6666666666666666)

-}

lInt3 = lI 1 5 [3, 9, 100, 101]
lInt4 = lI 1 5 [7, 3, 9, 100, 101]

delta_lInt_34 = ω lInt3 lInt4 1

lInt4' = applyForward lInt3 (getDelta delta_lInt_34)
lInt3' = applyBackward (getDelta delta_lInt_34) lInt4
{-

*TestData.Structured.Others.Integer2> delta_lInt_34
(Delta_List [chg. Delta_Prim 3, ((ƛx.x+4), (ƛx.x-4)), 7, 
             ins. 3, 
             chg. Delta_Prim (=9) , 
             chg. Delta_Prim (=100) , 
             chg. Delta_Prim (=101) 
            ]
,0.7111111111111111)

*TestData.Structured.Others.Integer2> lInt4'
[7, 3, 9, 100, 101]
*TestData.Structured.Others.Integer2> eq lInt4 lInt4'
True

*TestData.Structured.Others.Integer2> lInt3'
[3, 9, 100, 101]
*TestData.Structured.Others.Integer2> eq lInt3 lInt3'
True
-}

delta_lInt_34_half = ω lInt3 lInt4 0.5
{-

*TestData.Structured.Others.Integer2> delta_lInt_34_half
(Delta_List [chg. Delta_Prim 3, ((ƛx.x+4), (ƛx.x-4)), 7, 
             ins. 3, 
             chg. Delta_Prim (=9) , 
             chg. Delta_Prim (=100) , 
             chg. Delta_Prim (=101) 
            ]
,0.7111111111111111)

-}
