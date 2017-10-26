module TestData.Graph_EdgeSet where 

import Main.Value
import Main.Delta
import Main.Similarity
import Main.Omega

dublin_Irish = S 1 1 "Baile Atha Cliath"
galway_Irish = S 1 1 "Gaillimh"
cork_Irish = S 1 1 "Corcaigh"
belfast_Irish = S 1 1 "Beal Feirste"
derry_Irish = S 1 1 "Doire"

dublin_English = S 1 1 "Dublin"
dundalk_English = S 1 1 "Dundalk"
galway_English = S 1 1 "Galway"
cork_English = S 1 1 "Cork"
belfast_English = S 1 1 "Belfast"
derry_English = S 1 1 "Derry"


dublin1 = sR 1 0.5 [165.6, 208.6, 233.3, 255.3]
galway1 = sR 1 0.5 [208.6, 204]
cork1 = sR 1 0.5 [204, 255.3]
belfast1 = sR 1 0.5 [113, 165.6]
derry1 = sR 1 0.5 [113, 233.3]

dublin2 = sR 1 0.5 [82, 233.3, 209, 255]
dundalk = sR 1 0.5 [82, 85]
galway2 = sR 1 0.5 [204, 209]
cork2 = sR 1 0.5 [204, 255]
belfast2 = sR 1 0.5 [113, 85]
derry2 = sR 1 0.5 [113, 233.3]

dublinNode1 = Pair dublin_Irish dublin1
galwayNode1 = Pair galway_Irish galway1
corkNode1 = Pair cork_Irish cork1
belfastNode1 = Pair belfast_Irish belfast1
derryNode1 = Pair derry_Irish derry1

dublinNode2 = Pair dublin_English dublin2
dundalkNode = Pair dundalk_English dundalk
galwayNode2 = Pair galway_English galway2
corkNode2 = Pair cork_English cork2
belfastNode2 = Pair belfast_English belfast2
derryNode2 = Pair derry_English derry2


graph1 = setT [dublin1, galway1, cork1, belfast1, derry1]
graph2 = setT [dublin2, dundalk, galway2, cork2, belfast2, derry2]

{-
graph1 = setT [ dublinNode1
              , galwayNode1
              , corkNode1
              , belfastNode1
              , derryNode1
              ]
graph2 = setT [ dublinNode2
              , dundalkNode
              , galwayNode2
              , corkNode2
              , belfastNode2
              , derryNode2
              ]
-}
delta_graph_12 = ω graph1 graph2 1

{-
*TestData.Graph_EdgeSet> delta_graph_12
(Delta_Set [ Delta_Set [ Delta_Prim (ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)
                       , Delta_Prim = 
                       , Delta_Prim (ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)
                       , Delta_Prim (-165.6, +82.0) (+165.6, -82.0)
                       ]
           , Delta_Set [ Delta_Prim (ƛx.x+0.4000000000000057), (ƛx.x-0.4000000000000057)
                       , Delta_Prim = 
                       ]
           , Delta_Set [ Delta_Prim = 
                       , Delta_Prim (ƛx.x-0.30000000000001137), (ƛx.x+0.30000000000001137)
                       ]
           , Delta_Set [ Delta_Prim = 
                       , Delta_Prim (-165.6, +85.0) (+165.6, -85.0)
                       ]
           , Delta_Set [ Delta_Prim = , Delta_Prim = 
                       ]
           , +{82.0, 85.0}
           ]
,0.5538461538461485)

(319.78 secs, 85377771096 bytes)
-}
