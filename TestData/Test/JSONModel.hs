module TestData.JSONModel where 

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Omega
import Main.Solution

import Main.Application.Apply
import Main.Application.ApplySolution
import Main.Application.ApplyValue
import Main.Application.ApplyDelta
import Main.Application.Orig
import Main.Application.Targ
------------------------------------------------------
{-
-- JSON Example http://json.org/example.html

{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem1": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}

-- Modified version

{"menu": {
  "id": "file",
  "value": "file", -- changed point 1) File to file
  "popup": {
    "menuitem2": [ -- changed point 2) menuitem1 to menuitem2
      {"Value": "New", "Onclick": "CreateNewDoc()"},-- changed point 3) value to Value, onclick to Onclick
      {"Value": "Open", "Onclick": "OpenDoc()"},
      {"Value": "Close", "Onclick": "CloseDoc()"}
    ]
  }
}}
-}



jo11 = JObject [("value", (JString "New"))
              ,("onclick", (JString "CreateNewDoc()"))
              ]
jo12 = JObject [("value", (JString "Open"))
              ,("onclick", (JString "OpenDoc()"))
              ]
jo13 = JObject [("value", (JString "Close"))
              ,("onclick", (JString "CloseDoc()"))
              ]
jarray1 = JArray [jo11, jo12, jo13]

jo21 = JObject [("Value", (JString "New"))
              ,("Onclick", (JString "CreateNewDoc()"))
              ]
jo22 = JObject [("Value", (JString "Open"))
              ,("Onclick", (JString "OpenDoc()"))
              ]
jo23 = JObject [("Value", (JString "Close"))
              ,("Onclick", (JString "CloseDoc()"))
              ]
jarray2 = JArray [jo21, jo22, jo23]


jo_item1 = JObject [("menuitem1", jarray1)]
jo_item2 = JObject [("menuitem2", jarray2)]

jo_in_1 = JObject [("id", (JString "file"))
                 ,("value", (JString "File"))
                 ,("popup", jo_item1)
                 ]

jo_in_2 = JObject [("id", (JString "file"))
                 ,("value", (JString "file"))
                 ,("popup", jo_item2)
                 ]

jo_out_1 = JObject [("menu", jo_in_1)]
jo_out_2 = JObject [("menu", jo_in_2)]

json1 = JSON jo_out_1
json2 = JSON jo_out_2

{-
*TestData.JSON> json1
JObject [("menu",
          JObject [("id",JString "file"),
                   ("value",JString "File"),
                   ("popup",JObject [("menuitem1",
                                      JArray [JObject [("value",JString "New"),
                                                       ("onclick",JString "CreateNewDoc()")
                                                      ],
                                              JObject [("value",JString "Open"),
                                                       ("onclick",JString "OpenDoc()")
                                                      ],
                                              JObject [("value",JString "Close"),
                                                       ("onclick",JString "CloseDoc()")
                                                      ]
                                             ]
                                     )
                                    ]
                   )
                  ]
         )
        ]

*TestData.JSON> json2
JObject [("menu",
         JObject [("id",JString "file"),
                  ("value",JString "file"),
                  ("popup",JObject [("menuitem2",
                                     JArray [JObject [("Value",JString "New"),
                                                      ("Onclick",JString "CreateNewDoc()")
                                                     ],
                                             JObject [("Value",JString "Open"),
                                                      ("Onclick",JString "OpenDoc()")
                                                     ],
                                             JObject [("Value",JString "Close"),
                                                      ("Onclick",JString "CloseDoc()")
                                                     ]
                                            ]
                                    )
                                   ]
                 )
                ]
         )
        ]
-}

delta_json_12 = ω json1 json2 1

{-

*TestData.JSON> delta_json_12
(Delta_JSON (
 Delta_Set [chg. 
       Delta_Pair (Delta_Prim (="menu") ) 
                  (Delta_JSON (
                   Delta_Set [chg. 
                         Delta_Pair (Delta_Prim (="id") ) 
                                    (Delta_JSON (Delta_Prim (="file") )), 
                              chg. 
                         Delta_Pair (Delta_Prim (="value") ) 
                                    (Delta_JSON (Delta_Prim <+"file" ,-"File">)), 
                              chg. 
                         Delta_Pair (Delta_Prim (="popup") ) 
                                    (Delta_JSON (
                                     Delta_Set [chg. 
                                           Delta_Pair (Delta_Prim <+"menuitem2" ,-"menuitem1">) 
                                                      (Delta_JSON (
                                                       Delta_List [chg. Delta_JSON (
                                                                        Delta_Set [chg. 
                                                                              Delta_Pair (Delta_Prim <+"Value" ,-"value">) 
                                                                                         (Delta_JSON (Delta_Prim (="New") )), 
                                                                                   chg. 
                                                                                   Delta_Pair (Delta_Prim <+"Onclick" ,-"onclick">) 
                                                                                              (Delta_JSON (Delta_Prim (="CreateNewDoc()") ))
                                                                                   ]), 
                                                                   chg. Delta_JSON (
                                                                        Delta_Set [chg. 
                                                                              Delta_Pair (Delta_Prim <+"Value" ,-"value">) 
                                                                                         (Delta_JSON (Delta_Prim (="Open") )), 
                                                                                   chg. 
                                                                                   Delta_Pair (Delta_Prim <+"Onclick" ,-"onclick">) 
                                                                                              (Delta_JSON (Delta_Prim (="OpenDoc()") ))
                                                                                  ]), 
                                                                   chg. Delta_JSON (
                                                                        Delta_Set [chg. 
                                                                              Delta_Pair (Delta_Prim <+"Value" ,-"value">) 
                                                                                         (Delta_JSON (Delta_Prim (="Close") )), 
                                                                                   chg. 
                                                                              Delta_Pair (Delta_Prim <+"Onclick" ,-"onclick">) 
                                                                                         (Delta_JSON (Delta_Prim (="CloseDoc()") ))
                                                                                  ])
                                                                  ])
                                                      )
                                              ])
                                   )
                              ]
           ))])
,0.9268588137009189)
-}

json2' = applyForward json1 (getDelta delta_json_12)

json1' = applyBackward (getDelta delta_json_12) json2


{-

*TestData.JSON> json2
JSON: 
JObject 
[("menu",
JObject [("id",JString "file"),
         ("value",JString "file"),
         ("popup",
          JObject [("menuitem2",
                  JArray [JObject [("Value",JString "New"),
                                   ("Onclick",JString "CreateNewDoc()")],
                          JObject [("Value",JString "Open"),
                                   ("Onclick",JString "OpenDoc()")],
                          JObject [("Value",JString "Close"),
                                   ("Onclick",JString "CloseDoc()")]
                         ]
                   )
                  ]
         )
        ]
)]

*TestData.JSON> json2'
JSON: 
JObject 
[("menu",
JObject [("popup",
         JObject [("menuitem2",
                 JArray [JObject [("Value",JString "New"),
                                  ("Onclick",JString "CreateNewDoc()")],
                         JObject [("Value",JString "Open"),
                                  ("Onclick",JString "OpenDoc()")],
                         JObject [("Value",JString "Close"),
                                  ("Onclick",JString "CloseDoc()")]])
                  ]),
         ("id",JString "file"),
         ("value",JString "file")
         ]
)]

-}




d = getDeltaJSON (getDelta delta_json_12)

stepList = getStepList d

xs = getObj jo_out_1

test = applyForward (Set (ljo_to_set xs)) d 

origSet = convert_for_Apply (Set (ljo_to_set xs))-- the origSet is type of [T]
origDelta = orig_StepList stepList -- the origDelta is type of [(T,Step)]
applyForwardUnknown = ApplyUnknown origSet origDelta
matchingPairs = head_solution (applySolutions [applyForwardUnknown])
addition = getTypedSet (applyForward (Set EmptySet) (Delta_Set stepList))

extends0 = polish applyForwardUnknown

allPairs = tlist_tslist origSet origDelta

allWays = allways allPairs origSet origDelta

head_extends0 = head extends0

inserts0 = ins_Applycands [] extends0
{-
*TestData.JSON> head_extends0
Matching:[applyChg: ApplySingle
          (Pair ("menu",
          JObject [("id",JString "file"),
                   ("value",JString "File"),
                   ("popup",
                   JObject [("menuitem1",
                   JArray [JObject [("value",JString "New"),("onclick",JString "CreateNewDoc()")],
                           JObject [("value",JString "Open"),("onclick",JString "OpenDoc()")],
                           JObject [("value",JString "Close"),("onclick",JString "CloseDoc()")]])])]
                )
          Pair ("menu",
          JObject [("popup",JObject[("menuitem1",
                            JArray [JObject [("onclick",JString "CreateNewDoc()"),("value",JString "New")],
                                    JObject [("onclick",JString "OpenDoc()"),("value",JString "Open")],
                                    JObject [("onclick",JString "CloseDoc()"),("value",JString "Close")]
                                   ])
                                   ]
                   ),
                   ("value",JString "File"),
                   ("id",JString "file")
                  ]
               ) 
          -> chg. 
             Delta_Pair (Delta_Prim (="menu") ) 
                        (Delta_JSON (
                        Delta_Set [chg. 
                                   Delta_Pair (Delta_Prim (="id") ) 
                                              (Delta_JSON (Delta_Prim (="file") )), 
                                   chg. 
                                   Delta_Pair (Delta_Prim (="value") ) 
                                              (Delta_JSON (Delta_Prim <+"file" ,-"File">)), 
                                   chg. 
                                   Delta_Pair (Delta_Prim (="popup") ) 
                                              (Delta_JSON (
                                               Delta_Set [chg. 
                                                          Delta_Pair (Delta_Prim <+"menuitem2" ,-"menuitem1">) 
                                                                     (Delta_JSON (
                                                                      Delta_List [chg. 
                                                                                  Delta_JSON (
                                                                                  Delta_Set [chg. 
                                                                                             Delta_Pair (Delta_Prim <+"Value" ,-"value">) 
                                                                                                        (Delta_JSON (Delta_Prim (="New") )), 
                                                                                             chg. 
                                                                                             Delta_Pair (Delta_Prim <+"Onclick" ,-"onclick">) 
                                                                                                        (Delta_JSON (Delta_Prim (="CreateNewDoc()") ))]), 
                                                                                             chg. 
                                                                                             Delta_JSON (
                                                                                             Delta_Set [chg. 
                                                                                                        Delta_Pair (Delta_Prim <+"Value" ,-"value">) 
                                                                                                                   (Delta_JSON (Delta_Prim (="Open") )), 
                                                                                                        chg. 
                                                                                                        Delta_Pair (Delta_Prim <+"Onclick" ,-"onclick">) 
                                                                                                                   (Delta_JSON (Delta_Prim (="OpenDoc()") ))]), 
                                                                                                        chg. 
                                                                                                        Delta_JSON (
                                                                                                        Delta_Set [chg. 
                                                                                                                   Delta_Pair (Delta_Prim <+"Value" ,-"value">) 
                                                                                                                              (Delta_JSON (Delta_Prim (="Close") )), 
                                                                                                                   chg. 
                                                                                                                   Delta_Pair (Delta_Prim <+"Onclick" ,-"onclick">) 
                                                                                                                              (Delta_JSON (Delta_Prim (="CloseDoc()") ))])]))]))])))]



-}

x = getSingleFst (getApplyStep (getHead (getASL head_extends0)))

y = getSingleSnd (getApplyStep (getHead (getASL head_extends0)))

{-
*TestData.JSON> x
Pair (String:"menu",JObject [("id",JString "file"),("value",JString "File"),("popup",JObject [("menuitem1",JArray [JObject [("value",JString "New"),("onclick",JString "CreateNewDoc()")],JObject [("value",JString "Open"),("onclick",JString "OpenDoc()")],JObject [("value",JString "Close"),("onclick",JString "CloseDoc()")]])])])
*TestData.JSON> y
Pair (String:"menu",JObject [("popup",JObject [("menuitem1",JArray [JObject [("onclick",JString "CreateNewDoc()"),("value",JString "New")],JObject [("onclick",JString "OpenDoc()"),("value",JString "Open")],JObject [("onclick",JString "CloseDoc()"),("value",JString "Close")]])]),("value",JString "File"),("id",JString "file")])
-}

delta_x_y = ω x y 1

fst_x = first x
snd_x = second x

fst_y = first y
snd_y = second y

{-

*TestData.JSON> snd_x
JSON: JObject [("id",JString "file"),("value",JString "File"),("popup",JObject [("menuitem1",JArray [JObject [("value",JString "New"),("onclick",JString "CreateNewDoc()")],JObject [("value",JString "Open"),("onclick",JString "OpenDoc()")],JObject [("value",JString "Close"),("onclick",JString "CloseDoc()")]])])]

*TestData.JSON> snd_y
JSON: JObject [("popup",JObject [("menuitem1",JArray [JObject [("onclick",JString "CreateNewDoc()"),("value",JString "New")],JObject [("onclick",JString "OpenDoc()"),("value",JString "Open")],JObject [("onclick",JString "CloseDoc()"),("value",JString "Close")]])]),("value",JString "File"),("id",JString "file")]

-}
delta_snd_xy = ω snd_x snd_y 1

unknown = Unknown snd_x snd_y

ext0 = refine unknown
ins0 = ins_cands [] ext0

ext1 = refine (head ins0)
ins1 = ins_cands (tail ins0) ext1

ext2 = refine (head ins1)
ins2 = ins_cands (tail ins1) ext2

ext3 = refine (head ins2)
ins3 = ins_cands (tail ins2) ext3

ext4 = refine (head ins3)
ins4 = ins_cands (tail ins3) ext4

ext5 = refine (head ins4)
ins5 = ins_cands (tail ins4) ext5

ext6 = refine (head ins5)
ins6 = ins_cands (tail ins5) ext6

ext7 = refine (head ins6)
ins7 = ins_cands (tail ins6) ext7

ext8 = refine (head ins7)
ins8 = ins_cands (tail ins7) ext8

ext9 = refine (head ins8)
ins9 = ins_cands (tail ins8) ext9

ext10 = refine (head ins9)
ins10 = ins_cands (tail ins9) ext10


{-
*TestData.JSON> ext9
[Delta_JSON (Delta_Set [chg. Delta_Pair (Unknown String:"popup" String:"id") 
                                        (Unknown JSON: JObject [("menuitem1",JArray [JObject [("value",JString "New"),("onclick",JString "CreateNewDoc()")],JObject [("value",JString "Open"),("onclick",JString "OpenDoc()")],JObject [("value",JString "Close"),("onclick",JString "CloseDoc()")]])] 
                                                 JSON: JString "file"
                                        )
                                      , unknownRest]
            )
]

*TestData.JSON> ext10
[Delta_JSON (Delta_Set [chg. Delta_Pair (Delta_Prim <+"popup" ,-"id">) *** Exception: Different JSON Values cannot be compared.
-}


module TestData.JSONModel where 

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Omega
import Main.Solution

import Main.Application.Apply
import Main.Application.ApplySolution
import Main.Application.ApplyValue
import Main.Application.ApplyDelta
import Main.Application.Orig
import Main.Application.Targ
------------------------------------------------------
{-
-- JSON Example http://json.org/example.html

{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem1": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}

-- Modified version

{"menu": {
  "id": "file",
  "value": "file", -- changed point 1) File to file
  "popup": {
    "menuitem2": [ -- changed point 2) menuitem1 to menuitem2
      {"Value": "New", "Onclick": "CreateNewDoc()"},-- changed point 3) value to Value, onclick to Onclick
      {"Value": "Open", "Onclick": "OpenDoc()"},
      {"Value": "Close", "Onclick": "CloseDoc()"}
    ]
  }
}}
-}

menu = S 1 1 "menu"
idStr = S 1 1 "id"
file = S 1 1 "file"
file_Cap = S 1 1 "File"
value = S 1 1 "value"
value_Cap = S 1 1 "Value"
popup = S 1 1 "popup"
menuitem1 = S 1 1 "menuitem1"
menuitem2 = S 1 1 "menuitem2"
new_Cap = S 1 1 "New"
open_Cap = S 1 1 "Open"
close_Cap = S 1 1 "Close"
onclick = S 1 1 "onclick"
createNewDoc = S 1 1 "CreateNewDoc()"
openDoc = S 1 1 "OpenDoc()"
closeDoc = S 1 1 "CloseDoc()"

jo11 = setT [(Pair value new_Cap), (Pair onclick createNewDoc)]
jo12 = setT [(Pair value open_Cap), (Pair onclick openDoc)]
jo13 = setT [(Pair value close_Cap), (Pair onclick closeDoc)]

jarray1 = listT [jo11, jo12, jo13]

jo_menuitem1 = setT [(Pair menuitem1 jarray1)]

jo_menu1 = setT [(Pair idStr file), (Pair value file_Cap), (Pair popup jo_menuitem1)]

json1 = setT [(Pair menu jo_menu1)]
