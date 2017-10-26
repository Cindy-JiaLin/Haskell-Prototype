module TestData.HumanRights2 where

import Data.List

import Model.Text

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

unorg_2 = "Article 1.\nAll human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood.\nArticle 2.\nEveryone is entitled to all the rights and freedoms set forth in this Declaration, without distinction of any kind, such as race, colour, sex, language, religion, political or other opinion, national or social origin, property, birth or other status. Furthermore, no distinction shall be made on the basis of the political, jurisdictional or international status of the country or territory to which a person belongs, whether it be independent, trust, non-self-governing or under any other limitation of sovereignty."

buzzlecom_2 = "Article 1 -- Right To Equality\nAll human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood.\nArticle 2 -- Freedom From Discrimination\nEveryone is entitled to all the rights and freedoms set forth in this Declaration, without distinction of any kind, such as race, color, sex, language, religion, political or other opinion, national or social origin, property, birth or other status. Furthermore, no distinction shall be made on the basis of the political, jurisdictional or international status of the country or territory to which a person belongs, whether it be independent, trust, non-self-governing or under any other limitation of sovereignty."

list1 = lText 1 3 (lines unorg_2)
list2 = lText 1 3 (lines buzzlecom_2)

delta_2 = ω list1 list2 1
delta_2_half = ω list1 list2 0.5


{-
*TestData.HumanRights2> delta_2
(Delta_List [ Delta_List [ Delta_Prim = 
                         , Delta_Prim "1" "1."
                         , +"--"
                         , +"Right"
                         , +"To"
                         , +"Equality"
                         ]
            , Delta_List [ Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         ]
            , Delta_List [ Delta_Prim = 
                         , Delta_Prim "2" "2."
                         , +"--"
                         , +"Freedom"
                         , +"From"
                         , +"Discrimination"
                         ]
            , Delta_List [ 
                           Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim "color," "colour,"
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = 
                         ]
             ]
,0.9577807250221044)
(22.90 secs, 4628594528 bytes)

*TestData.HumanRights2> delta_2_half
(Delta_List [ Delta_List [ Delta_Prim = 
                         , Delta_Prim "1" "1."
                         , +"--"
                         , +"Right"
                         , +"To"
                         , +"Equality"
                         ]
            , Delta_List [ Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim =  
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         ]
            , Delta_List [ Delta_Prim = 
                         , Delta_Prim "2" "2."
                         , +"--"
                         , +"Freedom"
                         , +"From"
                         , +"Discrimination"
                         ]
            , Delta_List [ Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim = , Delta_Prim = 
                         , Delta_Prim "color," "colour,"
                         , Delta_Prim = , Delta_Prim = 
                         , unknownListRest
                         ]
],0.5008841732979663)
(4.45 secs, 1213481600 bytes)

-}




