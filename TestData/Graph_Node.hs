module TestData.Graph_Node where 

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Omega

------------------------------------------------------

link :: Double -> String -> T
link d s = Pair (R 1 0.5 d) (S 0 1 s)

dublinA = setT [(link 165.6 "Baile Atha Cliath"), (link 208.6 "Gaillimh"),
                 (link 233.3 "Doire"), (link 255.3 "Corcaigh")
                ]
galwayA = setT [(link 208.6 "Baile Atha Cliath"), (link 204 "Corcaigh")]
corkA = setT [(link 204 "Gaillimh"), (link 255.3 "Baile Atha Cliath")]
belfastA = setT [(link 113 "Doire"), (link 165.6 "Baile Atha Cliath")]
derryA = setT [(link 113 "Beal Feirste"), (link 233.3 "Baile Atha Cliath")]


dublinB = setT [(link 82 "Dundalk"), (link 209 "Galway"), 
                (link 233.3 "Derry"),(link 255 "Cork")
               ]
dundalk = setT [(link 82 "Dublin"), (link 85 "Belfast")]
galwayB = setT [(link 209 "Dublin"), (link 204 "Cork")]
corkB = setT [(link 204 "Galway"), (link 255 "Dublin")]
belfastB = setT [(link 113 "Derry"), (link 85 "Dundalk")]
derryB = setT [(link 113 "Belfast"), (link 233.3 "Dublin")]


node :: String -> T -> T
node s x = Pair (S 0 1 s) x

graphA = setT [ (node "Baile Atha Cliath" dublinA)
              , (node "Gaillimh" galwayA)
              , (node "Corcaigh" corkA)
              , (node "Beal Feirste" belfastA)
              , (node "Doire" derryA)
              ]

graphB = setT [ (node "Dublin" dublinB)
              , (node "Dundalk" dundalk)
              , (node "Galway" galwayB)
              , (node "Cork" corkB)
              , (node "Belfast" belfastB)
              , (node "Derry" derryB)
              ]

delta_graph_AB = ω graphA graphB 1


{-

*TestData.Graph_Node> graphA
Set {
Pair (S 0.0 1 "Doire") 
     (Set {Pair (R 1.0 0.5 233.3) (S 0.0 1 "Baile Atha Cliath"), 
           Pair (R 1.0 0.5 113.0) (S 0.0 1 "Beal Feirste")
          }), 
Pair (S 0.0 1 "Beal Feirste") 
     (Set {Pair (R 1.0 0.5 165.6) (S 0.0 1 "Baile Atha Cliath"), 
           Pair (R 1.0 0.5 113.0) (S 0.0 1 "Doire")
          }), 
Pair (S 0.0 1 "Corcaigh") 
     (Set {Pair (R 1.0 0.5 255.3) (S 0.0 1 "Baile Atha Cliath"), 
           Pair (R 1.0 0.5 204.0) (S 0.0 1 "Gaillimh")
          }), 
Pair (S 0.0 1 "Gaillimh") 
     (Set {Pair (R 1.0 0.5 204.0) (S 0.0 1 "Corcaigh"), 
           Pair (R 1.0 0.5 208.6) (S 0.0 1 "Baile Atha Cliath")
          }), 
Pair (S 0.0 1 "Baile Atha Cliath") 
     (Set {Pair (R 1.0 0.5 255.3) (S 0.0 1 "Corcaigh"), 
           Pair (R 1.0 0.5 233.3) (S 0.0 1 "Doire"), 
           Pair (R 1.0 0.5 208.6) (S 0.0 1 "Gaillimh"), 
           Pair (R 1.0 0.5 165.6) (S 0.0 1 "Beal Feirste")
          })
}

*TestData.Graph_Node> graphB
Set {
Pair (S 0.0 1 "Derry") 
     (Set {Pair (R 1.0 0.5 233.3) (S 0.0 1 "Dublin"), 
           Pair (R 1.0 0.5 113.0) (S 0.0 1 "Belfast")
          }), 
Pair (S 0.0 1 "Belfast") 
     (Set {Pair (R 1.0 0.5 85.0) (S 0.0 1 "Dundalk"), 
           Pair (R 1.0 0.5 113.0) (S 0.0 1 "Derry")
          }), 
Pair (S 0.0 1 "Cork") 
     (Set {Pair (R 1.0 0.5 255.0) (S 0.0 1 "Dublin"), 
           Pair (R 1.0 0.5 204.0) (S 0.0 1 "Galway")
          }), 
Pair (S 0.0 1 "Galway") 
     (Set {Pair (R 1.0 0.5 204.0) (S 0.0 1 "Cork"), 
           Pair (R 1.0 0.5 209.0) (S 0.0 1 "Dublin")
          }), 
Pair (S 0.0 1 "Dundalk") 
     (Set {Pair (R 1.0 0.5 85.0) (S 0.0 1 "Belfast"), 
           Pair (R 1.0 0.5 82.0) (S 0.0 1 "Dublin")
          }), 
Pair (S 0.0 1 "Dublin") 
     (Set {Pair (R 1.0 0.5 255.0) (S 0.0 1 "Cork"), 
           Pair (R 1.0 0.5 233.3) (S 0.0 1 "Derry"), 
           Pair (R 1.0 0.5 209.0) (S 0.0 1 "Galway"), 
           Pair (R 1.0 0.5 82.0) (S 0.0 1 "Dundalk")
          })
}

*TestData.Graph_Node> delta_graph_AB
(Δ_Set [chg. 
        Δ_Pair (Δ_Prim <"Doire" => "Derry"> ) 
               (Δ_Set [chg. 
                       Δ_Pair (Id 233.3) 
                              (Δ_Prim <"Baile Atha Cliath" => "Dublin"> ), 
                       chg. 
                       Δ_Pair (Id 113.0) 
                              (Δ_Prim <"Beal Feirste" => "Belfast"> )
                      ]), 
        chg. 
        Δ_Pair (Δ_Prim <"Beal Feirste" => "Belfast"> ) 
               (Δ_Set [chg. 
                       Δ_Pair (Id 113.0) 
                              (Δ_Prim <"Doire" => "Derry"> ), 
                       del. (165.6,"Baile Atha Cliath"), 
                       ins. (85.0,"Dundalk")
                      ]), 
        chg. 
        Δ_Pair (Δ_Prim <"Corcaigh" => "Cork"> ) 
               (Δ_Set [chg. 
                       Δ_Pair (Δ_Prim 255.3, ((ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)), 255.0) 
                              (Δ_Prim <"Baile Atha Cliath" => "Dublin"> ), 
                       chg. Δ_Pair (Id 204.0) 
                                   (Δ_Prim <"Gaillimh" => "Galway"> )
                      ]), 
        chg. 
        Δ_Pair (Δ_Prim <"Gaillimh" => "Galway"> ) 
               (Δ_Set [chg. 
                       Δ_Pair (Id 204.0) 
                              (Δ_Prim <"Corcaigh" => "Cork"> ), 
                       chg. 
                       Δ_Pair (Δ_Prim 208.6, ((ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)), 209.0) 
                              (Δ_Prim <"Baile Atha Cliath" => "Dublin"> )
                      ]), 
        chg. 
        Δ_Pair (Δ_Prim <"Baile Atha Cliath" => "Dublin"> ) 
               (Δ_Set [chg. 
                       Δ_Pair (Δ_Prim 255.3, ((ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)), 255.0) 
                              (Δ_Prim <"Corcaigh" => "Cork"> ), 
                       chg. 
                       Δ_Pair (Id 233.3) 
                              (Δ_Prim <"Doire" => "Derry"> ), 
                       chg. 
                       Δ_Pair (Δ_Prim 208.6, ((ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)), 209.0) 
                              (Δ_Prim <"Gaillimh" => "Galway"> ), 
                       del. (165.6,"Baile Atha Cliath"), 
                       ins. (82.0,"Dundalk")]), 
                       ins. ("Dundalk",{(85.0,"Belfast"), (82.0,"Dublin")})
]
,0.5538461538461485)
(3.49 secs, 646,838,664 bytes)

-}

