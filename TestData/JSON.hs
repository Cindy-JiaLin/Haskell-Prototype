module TestData.JSON where 

import Model.JValue

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
delta_json_11 = ω json1 json1 1

intend_json1 = applyForward json1 (getDelta delta_json_11)

{-

*TestData.JSON> delta_json_11
(Id {("menu",{("id","file"), 
              ("value","File"), 
              ("popup",{("menuitem1",[{("value","New"), ("onclick","CreateNewDoc()")}, 
                                      {("value","Open"), ("onclick","OpenDoc()")}, 
                                      {("value","Close"), ("onclick","CloseDoc()")}
                                     ]
                        )
                       }
              )
             }
    )}
,1.0)

*TestData.JSON> intend_json1
JObject [("menu",
         JObject [("id",JString "file"),
                  ("value",JString "File"),
                  ("popup",
                   JObject [("menuitem1",
                            JArray [JObject [("value",JString "New"),("onclick",JString "CreateNewDoc()")],
                                    JObject [("value",JString "Open"),("onclick",JString "OpenDoc()")],
                                    JObject [("value",JString "Close"),("onclick",JString "CloseDoc()")]
                                   ]
                           )]
                  )
                ]
         )
        ]
*TestData.JSON> eq json1 intend_json1
True

-}

delta_json_12 = ω json1 json2 1
{-

*TestData.JSON> delta_json_12
(Δ_JSON (
 Δ_Set [chg. 
        Δ_Pair (Id "menu") 
               (Δ_JSON (
                Δ_Set [chg. Id ("id","file"), 
                       chg. 
                       Δ_Pair (Id "value") 
                              (Δ_JSON (Δ_Prim <"File" => "file"> )), 
                       chg. 
                       Δ_Pair (Id "popup") 
                              (Δ_JSON (
                               Δ_Set [chg. 
                                      Δ_Pair (Δ_Prim <"menuitem1" => "menuitem2"> ) 
                                             (Δ_JSON (
                                              Δ_List [chg. 
                                                      Δ_JSON (
                                                      Δ_Set [chg. 
                                                             Δ_Pair (Δ_Prim <"value" => "Value"> ) 
                                                                    (Id "New"), 
                                                             chg. 
                                                             Δ_Pair (Δ_Prim <"onclick" => "Onclick"> ) 
                                                                    (Id "CreateNewDoc()")
                                                            ]), 
                                                      chg. 
                                                      Δ_JSON (
                                                      Δ_Set [chg. 
                                                             Δ_Pair (Δ_Prim <"value" => "Value"> ) 
                                                                    (Id "Open"), 
                                                             chg. 
                                                             Δ_Pair (Δ_Prim <"onclick" => "Onclick"> ) 
                                                                    (Id "OpenDoc()")
                                                            ]), 
                                                      chg. 
                                                      Δ_JSON (
                                                      Δ_Set [chg. 
                                                             Δ_Pair (Δ_Prim <"value" => "Value"> ) 
                                                                    (Id "Close"), 
                                                             chg. 
                                                             Δ_Pair (Δ_Prim <"onclick" => "Onclick"> ) 
                                                                    (Id "CloseDoc()")
                                                            ])
                                                      ])
                                             )
                                       ])
                                 )
                        ])
                  )
             ])
,0.9268588137009189)
(0.34 secs, 84,757,616 bytes)
-}

json2' = applyForward json1 (getDelta delta_json_12)

json1' = applyBackward (getDelta delta_json_12) json2


{-

*TestData.JSON> json2'
JObject [("menu",
         JObject [("popup",
                  JObject [("menuitem2",
                           JArray [JObject [("Value",JString "New"),("Onclick",JString "CreateNewDoc()")],
                                   JObject [("Value",JString "Open"),("Onclick",JString "OpenDoc()")],
                                   JObject [("Value",JString "Close"),("Onclick",JString "CloseDoc()")]
                                  ]
                           )
                          ]
                  ),
                 ("value",JString "file"),
                 ("id",JString "file")
                ]
          )
        ]
(5.87 secs, 1,470,965,152 bytes)

*TestData.JSON> eq json2 json2'
True
(0.00 secs, 1,614,624 bytes)

*TestData.JSON> json1'
JObject [("menu",
          JObject [("popup",
                    JObject [("menuitem1",
                              JArray [JObject [("value",JString "New"),("onclick",JString "CreateNewDoc()")],
                                      JObject [("value",JString "Open"),("onclick",JString "OpenDoc()")],
                                      JObject [("value",JString "Close"),("onclick",JString "CloseDoc()")]])
                            ]),
                   ("value",JString "File"),
                   ("id",JString "file")
                  ]
        )]
(5.87 secs, 1,465,846,224 bytes)

*TestData.JSON> eq json1 json1'
True
(0.00 secs, 1,027,904 bytes)

-}



