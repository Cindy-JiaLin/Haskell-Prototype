module TestData.Structured.Others.Integer2 where

import Data.List

import Main.Value
import Main.Delta
import Main.Eq
import Main.Similarity
import Main.Solution
import Main.Omega

------------------------------------------------------------------------------------------------
-- Test Real Numbers

lInt1 = lI 1 2 [3, 9, 100, 101]
lInt2 = lI 1 2 [7, 3, 9, 100, 101]

delta_lInt_12 = ω lInt1 lInt2 1

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

{-

*TestData.Structured.Others.Integer2> delta_lInt_34
(Delta_List [chg. Delta_Prim 3, ((ƛx.x+4), (ƛx.x-4)), 7, 
             ins. 3, 
             chg. Delta_Prim (=9) , 
             chg. Delta_Prim (=100) , 
             chg. Delta_Prim (=101) 
            ]
,0.7111111111111111)

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
