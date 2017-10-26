module Main.Solution 
( solutions
, head_solution
, refine
) where

import Main.Value
import Main.Delta
import Main.Similarity

import Main.Eq
------------------------------------------------------------------------------------------------------------------------------------------
-- The refine function is refine a problem to a list of subproblems
-- Each constructor of Delta needs to be refined.
refine :: Delta -> [Delta]

refine (Unknown Unit Unit) =
       
       [(Id Unit)]

refine (Unknown (B w1 t1) (B w2 t2)) =
    
       if t1 == t2 then [(Id (B w1 t1))]

       else [(Δ_Prim (Forward (B w1 t1) (B w2 t2)) (Backward (B w1 t1) (B w2 t2)))]

refine (Unknown (C w1 x) (C w2 y)) =
    
       if x == y then [(Id (C w1 x))]

       else [(Δ_Prim (Forward (C w1 x) (C w2 y)) (Backward (C w1 x) (C w2 y)))]

refine (Unknown (Nat w1 x) (Nat w2 y)) =
    
       if x == y then [(Id (Nat w1 x))]

       else [(Δ_Prim (Forward (Nat w1 x) (Nat w2 y)) (Backward (Nat w1 x) (Nat w2 y)))]

refine (Unknown (R w1 n1 x) (R w2 n2 y)) = 
        
       if (eqDouble x y) then [(Id (R w1 n1 x))] 
   
       else [(Δ_Prim (Forward (R w1 n1 x) (R w2 n2 y)) (Backward (R w1 n1 x) (R w2 n2 y)))] 
 
refine (Unknown (I w1 n1 x) (I w2 n2 y)) =
 
       if x == y then [(Id (I w1 n1 x))] 
   
       else [(Δ_Prim (Forward (I w1 n1 x) (I w2 n2 y)) (Backward (I w1 n1 x) (I w2 n2 y)))]
 
refine (Unknown (S w1 n1 x) (S w2 n2 y)) =
      
       if x == y then [(Id (S w1 n1 x))] 

       else [(Δ_Prim (Forward (S w1 n1 x) (S w2 n2 y)) (Backward (S w1 n1 x) (S w2 n2 y)))] 
------------------------------------------------------------------------------------------------
-- Refine Unknown of two pairs 
-- When refine a unknown of two pairs, 
-- a Δ_Pair of two unknowns will be created.
-- The first unknown is composed of the first elements from two pairs.
-- The second unknown contains the second components of two pairs.
-- This refine go one step to get the Δ_Pair
-- To have a preparison for the refine the Δ_Pair
refine (Unknown (Pair x y) (Pair u v)) = 
       
       [(Δ_Pair (Unknown x u) (Unknown y v))]
-------------------------------------------------------------------------
-- Refine Unknown of two union types
refine (Unknown (Union (Lt x)) (Union (Lt y))) =

       [(Δ_Union (LL (Unknown x y)))]

refine (Unknown (Union (Rt x)) (Union (Rt y))) =

       [(Δ_Union (RR (Unknown x y)))]

refine (Unknown (Union (Lt x)) (Union (Rt y))) =
  
       [(Δ_Union (LR x y))]

refine (Unknown (Union (Rt x)) (Union (Lt y))) =

       [(Δ_Union (RL x y))]
-----------------------------------------------------------------------------------------------
-- Refine Unknown of two sets
refine (Unknown (Set EmptySet) (Set EmptySet)) =

       [(Δ_Set EmptyStepList)]


refine (Unknown (Set (Add x xs)) (Set EmptySet)) =
       
       let [(Δ_Set stepList)] = refine (Unknown (Set xs) (Set EmptySet))
        in [(Δ_Set (SL (Del x) stepList))]


refine (Unknown (Set EmptySet) (Set (Add y ys))) =
       
       let [(Δ_Set stepList)] = refine (Unknown (Set EmptySet) (Set ys))
        in [(Δ_Set (SL (Ins y) stepList))]

refine (Unknown (Set (Add x EmptySet)) (Set (Add y EmptySet))) =
       ins_cands []
       [ (Δ_Set (SL (Chg (Unknown x y)) EmptyStepList))
       , (Δ_Set (SL (Del x) (SL (Ins y) EmptyStepList)))
       , (Δ_Set (SL (Ins y) (SL (Del x) EmptyStepList)))
       ]

refine (Unknown (Set a) (Set b)) =      

       let all_pairs = more_to_more a b
        in ins_cands [] (allchoices all_pairs a b)
-----------------------------------------------------------------------------------------------
-- Refine Unknown of two multisets
refine (Unknown (MSet EmptyMSet) (MSet EmptyMSet)) =

       [(Δ_MSet EmptyStepList)]


refine (Unknown (MSet (Put x xs)) (MSet EmptyMSet)) =
       
       let [(Δ_MSet stepList)] = refine (Unknown (MSet xs) (MSet EmptyMSet))
        in [(Δ_MSet (SL (Del x) stepList))]


refine (Unknown (MSet EmptyMSet) (MSet (Put y ys))) =
       
       let [(Δ_MSet stepList)] = refine (Unknown (MSet EmptyMSet) (MSet ys))
        in [(Δ_MSet (SL (Ins y) stepList))]

refine (Unknown (MSet (Put x EmptyMSet)) (MSet (Put y EmptyMSet))) =

       ins_cands []
       [ (Δ_MSet (SL (Chg (Unknown x y)) EmptyStepList))
       , (Δ_MSet (SL (Del x) (SL (Ins y) EmptyStepList)))
       , (Δ_MSet (SL (Ins y) (SL (Del x) EmptyStepList)))
       ]

refine (Unknown (MSet a) (MSet b)) =      

       let all_pairs_mset = more_to_more_mset a b
        in ins_cands [] (allchoices_mset all_pairs_mset a b)

-------------------------------------------------------------------------------------------     
-- Refine Unknown of two lists
-- When refine a unknown of two empty lists,
-- an empty Δ_List will be generated.
refine (Unknown (List EmptyList) (List EmptyList)) =

       [(Δ_List EmptyStepList)]


refine (Unknown (List (Lst x xs)) (List EmptyList)) =
       
       let [(Δ_List stepList)] = refine (Unknown (List xs) (List EmptyList))
        in [(Δ_List (SL (Del x) stepList))]


refine (Unknown (List EmptyList) (List (Lst y ys))) =
       
       let [(Δ_List stepList)] = refine (Unknown (List EmptyList) (List ys))
        in [(Δ_List (SL (Ins y) stepList))]

refine (Unknown (List (Lst x EmptyList)) (List (Lst y EmptyList))) =
       ins_cands []
       [ (Δ_List (SL (Chg (Unknown x y)) EmptyStepList))
       , (Δ_List (SL (Del x) (SL (Ins y) EmptyStepList)))
       , (Δ_List (SL (Ins y) (SL (Del x) EmptyStepList)))
       ]

refine (Unknown (List (Lst x xs)) (List (Lst y ys))) =      
       ins_cands [] 
       [ (Δ_List (SL (Chg (Unknown x y)) (UnknownRest (List xs) (List ys))))
       , (Δ_List (SL (Del x) (UnknownRest (List xs) (List (Lst y ys)))))
       , (Δ_List (SL (Ins y) (UnknownRest (List (Lst x xs)) (List ys))))
       ]
-----------------------------------------------------------------------------------------------
-- Refine Unknown of two mappings
refine (Unknown (Mapping map1) (Mapping map2)) =      

       setDelta_to_mapDelta (refine (Unknown (Set (map_to_set map1)) (Set (map_to_set map2))))
------------------------------------------------------------------------------------------------
-- Refine Unknown of two objects of recursive types
refine (Unknown (Rec Unit) (Rec Unit)) =

       to_Delta_Rec (refine (Unknown Unit Unit))

refine (Unknown (Rec Unit) (Rec y)) =

       to_Delta_Rec (refine (Unknown (Union (Lt Unit)) (Union (Rt y))))

refine (Unknown (Rec x) (Rec Unit)) =

       to_Delta_Rec (refine (Unknown (Union (Lt x)) (Union (Rt Unit))))


refine (Unknown (Rec x) (Rec y)) =

       to_Delta_Rec (refine (Unknown x y))
--------------------------------------------------
refine (Unknown (JSON JNull) (JSON JNull)) =
        
       [(Id (JSON JNull))]

refine (Unknown (JSON (JBool x)) (JSON (JBool y))) =

       to_Delta_JSON (refine (Unknown (B 1 x) (B 1 y)))

refine (Unknown (JSON (JString x)) (JSON (JString y))) =

       to_Delta_JSON (refine (Unknown (S 1 3 x) (S 1 3 y)))

refine (Unknown (JSON (JNumber x)) (JSON (JNumber y))) =
       
       to_Delta_JSON (refine (Unknown (R 1 0.1 x) (R 1 0.1 y)))

refine (Unknown (JSON (JArray xs)) (JSON (JArray ys))) =

       to_Delta_JSON (refine (Unknown (List (ljv xs)) (List (ljv ys))))

refine (Unknown (JSON (JObject xs)) (JSON (JObject ys))) =

       to_Delta_JSON (refine (Unknown (Set (ljo_to_set xs)) (Set (ljo_to_set ys))))

refine (Unknown x y) =
       --error "JSON data has different JValues, in Solution.refine"
       [(Δ_JSON (Δ_Prim (Forward x y)(Backward x y)))]
------------------------------------------------------------------------------------------------
-- The Δ_Pair contains two components, 
-- each of them is of Delta type.
-- If the lower bound and upper bound of this Δ_Pair are not identical,
-- each component in this Δ_Pair needs to be compared, 
-- until the final solution of each delta can be confirmed.
refine (Δ_Pair x y) =

       [(Δ_Pair (head_solution (solutions 1 (refine x)))
                    (head_solution (solutions 1 (refine y)))
        )
       ]
-----------------------------------------------------------------------------------------------
-- Refine Δ_Union
refine (Δ_Union (LL d)) = 
        
       [(Δ_Union (LL (head_solution (solutions 1 (refine d)))))] 
 
refine (Δ_Union (RR d)) =
       
       [(Δ_Union (RR (head_solution (solutions 1 (refine d)))))] 

refine (Δ_Union _) = error "LR or RL union delta does not need to refined, since lwb = upb =0"  
-----------------------------------------------------------------------------------------------
-- Refine Δ_Set 
refine (Δ_Set stepList) = wrap_DeltaSet (refine_StepList stepList)   
-----------------------------------------------------------------------------------------------
-- Refine Δ_MSet 
refine (Δ_MSet stepList) = wrap_DeltaMSet (refine_StepList stepList)        
------------------------------------------------------------------------------------------------
-- Refine Δ_List 
refine (Δ_List stepList) = wrap_DeltaList (refine_StepList stepList)
-----------------------------------------------------------------------------------------------
-- Refine Δ_Map 
refine (Δ_Map stepList) = wrap_DeltaMap (refine_StepList stepList) 
------------------------------------------
-- Refine Δ_Rec
refine (Δ_Rec delta) = to_Delta_Rec (refine delta)
------------------------------------------
-- Refine Δ_JSON
refine (Δ_JSON delta) = to_Delta_JSON (refine delta)
------------------------------------------------------------------------------------
-- this refine_Step function is used to convert the Delta which has been refined
-- to be a list of Delta into a list of Step.
-- However (Ins x) and (Del x) have nothing to be refined.
refine_Step :: Step -> [Delta]
refine_Step (Chg d) = refine d
refine_Step _ = error "Ins or Del step does not need to refined, in Main.Solution."
------------------------------------------------------------------------------------
refine_StepList :: StepList -> [StepList]
refine_StepList (UnknownRest x y) = release_StepList (refine (Unknown x y)) 
refine_StepList EmptyStepList = []
refine_StepList (SL x xs) = 
                if fst (bounds_Step x) == snd (bounds_Step x)
                then extend_StepList x (refine_StepList xs)
                else combine_StepList (refine_Step x) xs 
------------------------------------------------------------------------------------
-- This CutPoint represents Cut-off Point of the overall control
type CutPoint = Double

isFinal :: Delta -> CutPoint -> Bool
isFinal d p = 
        ((fst (bounds_Delta d)) >= p) || ((fst (bounds_Delta d)) == (snd (bounds_Delta d)))
-----------------------------------------------------------------------------------------------
solutions :: CutPoint -> [Delta] -> [Delta]

solutions p [] = []

solutions p (current_delta:cands) =
            if (not (isFinal current_delta p))
            then let extends = refine current_delta
                     inserts = ins_cands cands extends
                  in solutions p inserts
            else current_delta:(solutions p cands)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- 4. This head_solution is used to get the first element from a non-empty list.
-- If the list is empty a error prompt will be presented.
head_solution :: [a] -> a 
head_solution [] = error "No solution!" 
head_solution (x:_) = x
