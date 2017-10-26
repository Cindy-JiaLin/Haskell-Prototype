module TestData.HumanRights1 where

import Data.List

import Model.Text

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Solution
import Main.Omega

unorg_1 = "Article 1.\nAll human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood."

buzzlecom_1 = "Article 1 -- Right To Equality\nAll human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood."

list1 = lText 1 3 (lines unorg_1)
list2 = lText 1 3 (lines buzzlecom_1)

delta_1 = ω list1 list2 1
delta_1_half = ω list1 list2 0.5

{-
*TestData.HumanRights1> delta_1
(Delta_List [ Delta_List [ Delta_Prim (id_F="Article"), (id_B="Article")
                         , Delta_Prim "1" "1."
                         , +"--"
                         , +"Right"
                         , +"To"
                         , +"Equality"
                         ]
            , Delta_List [ Delta_Prim (id_F="All"), (id_B="All")
                         , Delta_Prim (id_F="human"), (id_B="human")
                         , Delta_Prim (id_F="beings"), (id_B="beings")
                         , Delta_Prim (id_F="are"), (id_B="are")
                         , Delta_Prim (id_F="born"), (id_B="born")
                         , Delta_Prim (id_F="free"), (id_B="free")
                         , Delta_Prim (id_F="and"), (id_B="and")
                         , Delta_Prim (id_F="equal"), (id_B="equal")
                         , Delta_Prim (id_F="in"), (id_B="in")
                         , Delta_Prim (id_F="dignity"), (id_B="dignity")
                         , Delta_Prim (id_F="and"), (id_B="and")
                         , Delta_Prim (id_F="rights."), (id_B="rights.")
                         , Delta_Prim (id_F="They"), (id_B="They")
                         , Delta_Prim (id_F="are"), (id_B="are")
                         , Delta_Prim (id_F="endowed"), (id_B="endowed")
                         , Delta_Prim (id_F="with"), (id_B="with")
                         , Delta_Prim (id_F="reason"), (id_B="reason")
                         , Delta_Prim (id_F="and"), (id_B="and")
                         , Delta_Prim (id_F="conscience"), (id_B="conscience")
                         , Delta_Prim (id_F="and"), (id_B="and")
                         , Delta_Prim (id_F="should"), (id_B="should")
                         , Delta_Prim (id_F="act"), (id_B="act")
                         , Delta_Prim (id_F="towards"), (id_B="towards")
                         , Delta_Prim (id_F="one"), (id_B="one")
                         , Delta_Prim (id_F="another"), (id_B="another")
                         , Delta_Prim (id_F="in"), (id_B="in")
                         , Delta_Prim (id_F="a"), (id_B="a")
                         , Delta_Prim (id_F="spirit"), (id_B="spirit")
                         , Delta_Prim (id_F="of"), (id_B="of")
                         , Delta_Prim (id_F="brotherhood."), (id_B="brotherhood.")
                         ]
            ]
,0.9313725490196079)

(1.17 secs, 245719488 bytes)

*TestData.HumanRights1> delta_1_half

(Delta_List [ Delta_List 
                        [ Delta_Prim (id_F="Article"), (id_B="Article")
                        , Delta_Prim "1" "1."
                        , +"--"
                        , +"Right"
                        , +"To"
                        , +"Equality"
                        ]
           , Delta_List [ Delta_Prim (id_F="All"), (id_B="All")
                        , Delta_Prim (id_F="human"), (id_B="human")
                        , Delta_Prim (id_F="beings"), (id_B="beings")
                        , Delta_Prim (id_F="are"), (id_B="are")
                        , Delta_Prim (id_F="born"), (id_B="born")
                        , Delta_Prim (id_F="free"), (id_B="free")
                        , Delta_Prim (id_F="and"), (id_B="and")
                        , Delta_Prim (id_F="equal"), (id_B="equal")
                        , Delta_Prim (id_F="in"), (id_B="in")
                        , Delta_Prim (id_F="dignity"), (id_B="dignity")
                        , Delta_Prim (id_F="and"), (id_B="and")
                        , Delta_Prim (id_F="rights."), (id_B="rights.")
                        , Delta_Prim (id_F="They"), (id_B="They")
                        , Delta_Prim (id_F="are"), (id_B="are")
                        , Delta_Prim (id_F="endowed"), (id_B="endowed")
                        , Delta_Prim (id_F="with"), (id_B="with")
                        , unknownListRest
                        ]
          ]
,0.5196078431372549)

(0.34 secs, 92904272 bytes)

-}

