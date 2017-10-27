module Main.Application.Apply 
( applyForward
, applyBackward
, to_forward_TypedMSet
, to_backward_TypedMSet
) where

import Main.Value 
import Main.Delta
import Main.Similarity
import Main.Omega
import Main.Solution

import Main.Application.ApplyValue
import Main.Application.ApplyDelta
import Main.Application.ApplySolution

import Main.Application.Orig
import Main.Application.Targ

-----------------------------------------------------------------------------------------------
applyForward :: T -> Delta -> T

applyForward x (Id _) = x

applyForward (B w x) (Δ_Prim (Forward (B w1 t1) (B w2 t2)) _ ) = 
             if x == t1 then B w2 t2
             else B w x

applyForward (C w x) (Δ_Prim (Forward (C w1 c1) (C w2 c2)) _ ) = 
             if x == c1 then C w2 c2
             else C w x

applyForward (Nat w x) (Δ_Prim (Forward (Nat w1 n1) (Nat w2 n2)) _ ) = 
             if x == n1 then Nat w2 n2
             else Nat w x

applyForward (R w n x) (Δ_Prim (Forward (R w1 n1 r1) (R w2 n2 r2)) _ ) =
             if (n /= n1 ) || (n /= n2) 
             then error "applyForward: These real numbers have different accuracy"
             else if (sim_R x r1 n == 1)
                  then R w2 n2 r2
                  else if (sim_R r1 r2 n == 0)
                       then R w n x
                       else let res = x + r2 - r1
                             in R w n res 

applyForward (I w n x) (Δ_Prim (Forward (I w1 n1 i1) (I w2 n2 i2)) _ ) = 
             if (n /= n1 ) || (n /= n2) 
             then error "applyForward: These integers have different accuracy"
             else if (sim_I x i1 n == 1)
                  then I w2 n2 i2
                  else if (sim_I i1 i2 n == 0)
                       then I w n x
                       else let res = x + i2 - i1
                             in I w n res
            
applyForward (S w n x) (Δ_Prim (Forward (S w1 n1 s1) (S w2 n2 s2)) _) = 
             if (n /= n1 ) || (n /= n2) 
             then error "applyForward: These strings have different accuracy"
             else if (sim_S x s1 n == 1)
                  then S w2 n2 s2
                  else if (sim_S s1 s2 n == 0)
                       then S w n x
                       else S w2 n2 s2
-- applyForward Pairs
applyForward (Pair x y) (Δ_Pair delta_fst delta_snd) = 
          
             Pair (applyForward x delta_fst) (applyForward y delta_snd)

-- applyForward Union Types
applyForward (Union (Lt x)) (Δ_Union (LL d)) =

             Union (Lt (applyForward x d))

applyForward (Union (Lt x)) (Δ_Union (LR l r)) =

             if (getSimilarity (ω x l 1)) > 0.9
             then Union (Rt r)
             else Union (Lt x)

applyForward (Union (Lt x)) (Δ_Union _) = 
             
             error "Left side union type cannot be forward applied to the delta union from the right side."

applyForward (Union (Rt x)) (Δ_Union (RR d)) =

             Union (Rt (applyForward x d))

applyForward (Union (Rt x)) (Δ_Union (RL r l)) =
              
             if (getSimilarity (ω x r 1)) > 0.9
             then Union (Lt l)
             else Union (Rt x)

applyForward (Union (Rt x)) (Δ_Union _) =
           
             error "Right side union type cannot be forward applied to the delta union from the left side."            
 
-- applyForward Sets
applyForward (Set xs) (Δ_Set EmptyStepList) = Set xs

applyForward (Set EmptySet) (Δ_Set (SL (Ins y) stepList)) = 

             Set (Add y (getTypedSet (applyForward (Set EmptySet) (Δ_Set stepList))))

applyForward (Set EmptySet) (Δ_Set (SL (Chg (Δ_Prim (Forward x y) _)) stepList)) = 
             if getSimilarity (ω x y 1) == 0
             then Set (Add y (getTypedSet (applyForward (Set EmptySet) (Δ_Set stepList))))
             else applyForward (Set EmptySet) (Δ_Set stepList)

applyForward (Set EmptySet) (Δ_Set (SL _ stepList)) = 

             applyForward (Set EmptySet) (Δ_Set stepList) 

applyForward (Set xs) (Δ_Set stepList) =

             let origSet = convert_Set xs-- the origSet is type of [T]
                 origDelta = orig_StepList stepList -- the origDelta is type of [(T,Step)]
                 applyForwardUnknown = ApplyUnknown origSet origDelta
                 matchingPairs = head_solution (applySolutions [applyForwardUnknown])
                 addition = getTypedSet (applyForward (Set EmptySet) (Δ_Set stepList))
              in Set (union_TypedSet (to_forward_TypedSet (getASL matchingPairs)) addition)

-- applyForward Multisets
applyForward (MSet xs) (Δ_MSet EmptyStepList) = MSet xs

applyForward (MSet EmptyMSet) (Δ_MSet (SL (Ins y) stepList)) = 

             MSet (Put y (getTypedMSet (applyForward (MSet EmptyMSet) (Δ_MSet stepList))))

applyForward (MSet EmptyMSet) (Δ_MSet (SL (Chg (Δ_Prim (Forward x y) _)) stepList)) =
             
             if getSimilarity (ω x y 1) == 0
             then MSet (Put y (getTypedMSet (applyForward (MSet EmptyMSet) (Δ_MSet stepList))))
             else applyForward (MSet EmptyMSet) (Δ_MSet stepList)

applyForward (MSet EmptyMSet) (Δ_MSet (SL _ stepList)) =

             applyForward (MSet EmptyMSet) (Δ_MSet stepList)

applyForward (MSet xs) (Δ_MSet stepList) =

             let origMSet = convert_MSet xs-- the origSet is type of [T]
                 origMDelta = orig_StepList stepList -- the origDelta is type of [(T,Step)]
                 applyForwardUnknown = ApplyUnknown origMSet origMDelta
                 matchingPairs = head_solution (applySolutions [applyForwardUnknown])
                 addition = getTypedMSet (applyForward (MSet EmptyMSet) (Δ_MSet stepList))
              in MSet (mix_TypedMSet (to_forward_TypedMSet (getASL matchingPairs)) addition)  

-- applyForward Lists
applyForward (List xs) (Δ_List EmptyStepList) = List xs

applyForward (List EmptyList) (Δ_List (SL (Ins y) stepList)) = 

             List (Lst y (getTypedList (applyForward (List EmptyList) (Δ_List stepList))))

applyForward (List EmptyList) (Δ_List (SL (Chg (Δ_Prim (Forward x y) _)) stepList)) = 
             if getSimilarity (ω x y 1) == 0
             then List (Lst y (getTypedList (applyForward (List EmptyList) (Δ_List stepList))))
             else applyForward (List EmptyList) (Δ_List stepList)

applyForward (List EmptyList) (Δ_List (SL _ stepList)) = 

             applyForward (List EmptyList) (Δ_List stepList) 

applyForward (List xs) (Δ_List stepList) =

             let indexed_Set = Set (indexed_to_TypedSet xs)
                 indexed_origList = List (indexed_to_TypedList (orig_StepList stepList))
                 indexed_targList = List (indexed_to_TypedList (targ_StepList stepList))
                 indexed_Delta_Set = 
                   Δ_Set (getStepList (getDelta (ω indexed_origList indexed_targList 1)))
              in List (de_indexed (sortedSet (getTypedSet (applyForward indexed_Set indexed_Delta_Set))))  
-- applyForward Mappings
applyForward (Mapping xs) (Δ_Map stepList) =
             
             let setC = Set (map_to_set xs)
                 setD = Δ_Set stepList
              in Mapping (set_to_map (getTypedSet (applyForward setC setD)))

-- applyForward Objects of Recursive Types
applyForward (Rec x) (Δ_Rec d) = Rec (applyForward x d)

-- applyForward Objects of JSON
-- JSON Null:  applyForward (JSON Null) (Id _) = JSON Null
-- Since this is the same effect of applyForward x (Id _) = x
applyForward (JSON (JBool x)) (Δ_JSON d) = JSON (JBool (getBool (applyForward (B 1 x) d)))

applyForward (JSON (JString x)) (Δ_JSON d) = JSON (JString (getStr (applyForward (S 1 3 x) d)))

applyForward (JSON (JNumber x)) (Δ_JSON d) = JSON (JNumber (getReal (applyForward (R 1 0.1 x) d)))

applyForward (JSON (JArray xs)) (Δ_JSON d) = JSON (JArray (to_ljv (getTypedList (applyForward (List (ljv xs)) d))))

applyForward (JSON (JObject xs)) (Δ_JSON d) = 
             JSON (JObject (set_to_ljo (getTypedSet (applyForward (Set (ljo_to_set xs)) d)))) 
-----------------------------------------------------------------------------------------------
applyBackward :: Delta -> T -> T 

applyBackward (Id _) x = x
-- applyBackward Primitive Types

applyBackward (Δ_Prim _ (Backward (B w1 t1) (B w2 t2))) (B w x) = 
              if x == t2 then B w1 t1
              else B w x

applyBackward (Δ_Prim _ (Backward (C w1 c1) (C w2 c2))) (C w x) = 
              if x == c2 then C w1 c1
              else C w x

applyBackward (Δ_Prim _ (Backward (Nat w1 n1) (Nat w2 n2))) (Nat w x) = 
              if x == n2 then Nat w1 n1
              else Nat w x

applyBackward (Δ_Prim _ (Backward (R w1 n1 r1) (R w2 n2 r2))) (R w n x)=
             if (n /= n1 ) || (n /= n2) 
             then error "applyBackward: These real numbers have different accuracy"
             else if (sim_R x r2 n == 1)
                  then R w1 n1 r1
                  else if (sim_R r1 r2 n == 0)
                       then R w n x 
                       else let res = x + r1 - r2
                            in R w n res 

applyBackward (Δ_Prim _ (Backward (I w1 n1 i1) (I w2 n2 i2))) (I w n x) = 
             if (n /= n1 ) || (n /= n2) 
             then error "applyBackward: These integers have different accuracy"
             else if (sim_I x i2 n == 1)
                  then I w1 n1 i1
                  else if (sim_I i1 i2 n == 0)
                       then I w n x 
                       else let res = x + i1 - i2
                             in I w n res
            
applyBackward (Δ_Prim _ (Backward (S w1 n1 s1) (S w2 n2 s2))) (S w n x) = 
             if (n /= n1 ) || (n /= n2) 
             then error "applyBackward: These strings have different accuracy"
-- If x == s2, then s1 will be returned
             else if (sim_S x s2 n == 1)
                  then S w1 n1 s1
                  else if (sim_S s1 s2 n == 0)
                       then S w n x
                       else S w1 n1 s1

-- applyBackward Pairs
applyBackward (Δ_Pair delta_fst delta_snd) (Pair x y) =

              Pair (applyBackward delta_fst x) (applyBackward delta_snd y)

-- applyBackward Union Types
applyBackward (Δ_Union (LL d)) (Union (Lt x)) =

             Union (Lt (applyBackward d x))

applyBackward (Δ_Union (RL r l)) (Union (Lt x)) =

             if (getSimilarity (ω x l 1)) > 0.9
             then Union (Rt r)
             else Union (Lt x)

applyBackward (Δ_Union _) (Union (Lt x))= 
             
             error "Left side union type cannot be backward applied to the delta union from the left side."

applyBackward (Δ_Union (RR d)) (Union (Rt x)) =

             Union (Rt (applyBackward d x))

applyBackward (Δ_Union (LR l r)) (Union (Rt x)) =
              
             if (getSimilarity (ω x r 1)) > 0.9
             then Union (Lt l)
             else Union (Rt x)

applyBackward (Δ_Union _) (Union (Rt x)) =
           
             error "Right side union type cannot be backward applied to the delta union from the right side."            



-- applyBackward Sets
applyBackward (Δ_Set EmptyStepList) (Set ys) = Set ys

applyBackward (Δ_Set (SL (Del x) stepList)) (Set EmptySet)  = 

              Set (Add x (getTypedSet (applyBackward (Δ_Set stepList) (Set EmptySet))))

applyBackward (Δ_Set (SL (Chg (Δ_Prim (Forward x y) _)) stepList)) (Set EmptySet) =
           
              if getSimilarity (ω x y 1) == 0
              then Set (Add x (getTypedSet (applyBackward (Δ_Set stepList) (Set EmptySet))))
              else applyBackward (Δ_Set stepList) (Set EmptySet)

applyBackward (Δ_Set (SL _ stepList)) (Set EmptySet) =

              applyBackward (Δ_Set stepList) (Set EmptySet) 

applyBackward (Δ_Set stepList) (Set ys) =

              let targSet = convert_Set ys
                  targDelta = targ_StepList stepList
                  applyBackwardUnknown = ApplyUnknown targSet targDelta
                  matchingPairs = head_solution (applySolutions [applyBackwardUnknown])
                  addition = getTypedSet (applyBackward (Δ_Set stepList) (Set EmptySet))
               in Set (union_TypedSet (to_backward_TypedSet (getASL matchingPairs)) addition) 

-- applyBackward Multisets
applyBackward (Δ_MSet EmptyStepList) (MSet ys) = MSet ys

applyBackward (Δ_MSet (SL (Del x) stepList)) (MSet EmptyMSet)  = 

              MSet (Put x (getTypedMSet (applyBackward (Δ_MSet stepList) (MSet EmptyMSet))))

applyBackward (Δ_MSet (SL (Chg (Δ_Prim (Forward x y) _)) stepList)) (MSet EmptyMSet) =
              if getSimilarity (ω x y 1) == 0
              then MSet (Put x (getTypedMSet (applyBackward (Δ_MSet stepList) (MSet EmptyMSet)))) 
              else applyBackward (Δ_MSet stepList) (MSet EmptyMSet)

applyBackward (Δ_MSet (SL _ stepList)) (MSet EmptyMSet) =

              applyBackward (Δ_MSet stepList) (MSet EmptyMSet) 

applyBackward (Δ_MSet stepList) (MSet ys) =

             let targMSet = convert_MSet ys
                 targMDelta = targ_StepList stepList
                 applyBackwardUnknown = ApplyUnknown targMSet targMDelta
                 matchingPairs = head_solution (applySolutions [applyBackwardUnknown])
                 addition = getTypedMSet (applyBackward (Δ_MSet stepList) (MSet EmptyMSet))
              in MSet (mix_TypedMSet (to_backward_TypedMSet (getASL matchingPairs)) addition) 

-- applyBackward Lists
applyBackward (Δ_List EmptyStepList) (List ys) = List ys

applyBackward (Δ_List (SL (Del x) stepList)) (List EmptyList)  = 

              List (Lst x (getTypedList (applyBackward (Δ_List stepList) (List EmptyList))))

applyBackward (Δ_List (SL (Chg (Δ_Prim (Forward x y) _)) stepList)) (List EmptyList) =
           
              if getSimilarity (ω x y 1) == 0
              then List (Lst x (getTypedList (applyBackward (Δ_List stepList) (List EmptyList))))
              else applyBackward (Δ_List stepList) (List EmptyList)

applyBackward (Δ_List (SL _ stepList)) (List EmptyList) =

              applyBackward (Δ_List stepList) (List EmptyList) 

applyBackward (Δ_List stepList) (List xs)=

              let indexed_Set = Set (indexed_to_TypedSet xs)
                  indexed_origList = List (indexed_to_TypedList (orig_StepList stepList))
                  indexed_targList = List (indexed_to_TypedList (targ_StepList stepList))
                  indexed_Delta_Set = 
                    Δ_Set (getStepList (getDelta (ω indexed_origList indexed_targList 1)))
               in List (de_indexed (sortedSet (getTypedSet (applyBackward indexed_Delta_Set indexed_Set))))

-- applyBackward Mappings
applyBackward (Δ_Map stepList) (Mapping xs) =
             
             let setC = Set (map_to_set xs)
                 setD = Δ_Set stepList
              in Mapping (set_to_map (getTypedSet (applyBackward setD setC)))

-- applyBackward Objects of Recursive Types
applyBackward (Δ_Rec d) (Rec x) = Rec (applyBackward d x)

-- applyBackward Objects of JSON 
-- JSON Null:  applyBackward (Id _) (JSON Null) = JSON Null

applyBackward (Δ_JSON d) (JSON (JBool x)) = JSON (JBool (getBool (applyBackward d (B 1 x))))

applyBackward (Δ_JSON d) (JSON (JString x)) = JSON (JString (getStr (applyBackward d (S 1 3 x))))

applyBackward (Δ_JSON d) (JSON (JNumber x)) = JSON (JNumber (getReal (applyBackward d (R 1 0.1 x))))

applyBackward (Δ_JSON d) (JSON (JArray xs)) = JSON (JArray (to_ljv (getTypedList (applyBackward d (List (ljv xs))))))

applyBackward (Δ_JSON d) (JSON (JObject xs)) = 
              JSON (JObject (set_to_ljo (getTypedSet (applyBackward d (Set (ljo_to_set xs))))))

------------------------------------------------------------------------------------------
-- This to_forward_TypedSet function

to_forward_TypedSet :: ApplyStepList -> TypedSet
to_forward_TypedSet ApplyEmptyStepList = EmptySet
to_forward_TypedSet (ApplySL (ApplyDel x) asl) = 
                    Add x (to_forward_TypedSet asl)
to_forward_TypedSet (ApplySL (ApplyIns y) asl) =
                    to_forward_TypedSet asl
to_forward_TypedSet (ApplySL (ApplyChg (ApplySingle x (y,step))) asl) =
                    if getSimilarity (ω x y 1) == 0
                    then add x (to_forward_TypedSet asl)
                    else let single_set = applyForwardStep x step
                          in if isEmptySet single_set 
                             then to_forward_TypedSet asl
                             else Add (pop single_set) (to_forward_TypedSet asl)

to_backward_TypedSet :: ApplyStepList -> TypedSet
to_backward_TypedSet ApplyEmptyStepList = EmptySet
to_backward_TypedSet (ApplySL (ApplyDel x) asl) = 
                     Add x (to_backward_TypedSet asl)
to_backward_TypedSet (ApplySL (ApplyIns y) asl) =
                     to_backward_TypedSet asl
to_backward_TypedSet (ApplySL (ApplyChg (ApplySingle x (y,step))) asl) =
                     if getSimilarity (ω x y 1) == 0
                     then add x (to_backward_TypedSet asl)
                     else let single_set = applyBackwardStep x step
                           in if isEmptySet single_set 
                              then to_backward_TypedSet asl
                              else Add (pop single_set) (to_backward_TypedSet asl)
-------------------------------------------------------------------------------------
-- This applyForwardStep function is used to apply an element to its corresponding edit operation
-- In particular, the forward function only considered about the change and deletion
-- applyTtoE
-- This result set contains only one element
applyForwardStep :: T -> Step -> TypedSet
applyForwardStep a (Del x) = EmptySet
applyForwardStep a (Chg (Δ_Prim (Forward x y) b)) =
                   if getSimilarity (ω x y 1) == 0
                   then EmptySet
                   else Add (applyForward a (Δ_Prim (Forward x y) b)) EmptySet
applyForwardStep a (Chg d) = Add (applyForward a d) EmptySet
applyForwardStep a _ = error "No insertion considered in forward application"

-- The backward function will deal with the change and insertion rahter than the deletion edit operation.
-- applyEtoT
-- This result set contains only one element
applyBackwardStep :: T -> Step -> TypedSet
applyBackwardStep a (Ins x) = EmptySet
applyBackwardStep a (Chg (Δ_Prim (Forward x y) b)) =
                    if getSimilarity (ω x y 1) == 0
                    then EmptySet
                    else Add (applyBackward (Δ_Prim (Forward x y) b) a) EmptySet
applyBackwardStep a (Chg d) = Add (applyBackward d a) EmptySet
applyBackwardStep a _ = error "No deletion considered in backward application"

-- This pop function is used to pop an element from a set
pop :: TypedSet -> T
pop EmptySet = error "pop function cannot be apply to an empty set"
pop (Add x xs) = x

------------------------------------------------------------------------------------------------
-- This to_forward_TypedMSet function is used to
-- get the final result multiset from a applyStepList
-- Only deletion and change are considered.
to_forward_TypedMSet :: ApplyStepList -> TypedMSet
to_forward_TypedMSet ApplyEmptyStepList = EmptyMSet
to_forward_TypedMSet (ApplySL (ApplyDel x) asl) = 
                     Put x (to_forward_TypedMSet asl)
to_forward_TypedMSet (ApplySL (ApplyIns y) asl) =
                     to_forward_TypedMSet asl
to_forward_TypedMSet (ApplySL (ApplyChg (ApplySingle x (y,step))) asl) =
                     if getSimilarity (ω x y 1) == 0
                     then put x (to_forward_TypedMSet asl)
                     else let single_mset = applyForwardStep_mset x step
                           in if isEmptyMSet single_mset 
                              then to_forward_TypedMSet asl
                              else put (pop_mset single_mset) (to_forward_TypedMSet asl)

to_backward_TypedMSet :: ApplyStepList -> TypedMSet
to_backward_TypedMSet ApplyEmptyStepList = EmptyMSet
to_backward_TypedMSet (ApplySL (ApplyDel x) asl) = 
                      Put x (to_backward_TypedMSet asl)
to_backward_TypedMSet (ApplySL (ApplyIns y) asl) =
                      to_backward_TypedMSet asl
to_backward_TypedMSet (ApplySL (ApplyChg (ApplySingle x (y,step))) asl) =
                      if getSimilarity (ω x y 1) == 0
                      then put x (to_backward_TypedMSet asl)
                      else let single_mset = applyBackwardStep_mset x step
                            in if isEmptyMSet single_mset 
                               then to_backward_TypedMSet asl
                               else put (pop_mset single_mset) (to_backward_TypedMSet asl)
----------------------------------------------------------------------------------------------------------
-- This applyForwardStep_mset function is used to get a multiset by
-- applying this function to an element to its corresponding edit operation
-- This result multiset only contains one element
applyForwardStep_mset :: T -> Step -> TypedMSet
applyForwardStep_mset a (Del x) = EmptyMSet
applyForwardStep_mset a (Chg (Δ_Prim (Forward x y) b)) =
                      if getSimilarity (ω x y 1) == 0
                      then EmptyMSet
                      else Put (applyForward a (Δ_Prim (Forward x y) b)) EmptyMSet
applyForwardStep_mset a (Chg d) = Put (applyForward a d) EmptyMSet
applyForwardStep_mset a _ = error "No insertion considered in forward application"
-- This result multiset only contains one element
-- The difference from the above function is this function is backward apply
applyBackwardStep_mset :: T -> Step -> TypedMSet
applyBackwardStep_mset a (Ins x) = EmptyMSet
applyBackwardStep_mset a (Chg (Δ_Prim (Forward x y) b)) =
                       if getSimilarity (ω x y 1) == 0
                       then EmptyMSet
                       else Put (applyBackward (Δ_Prim (Forward x y) b) a) EmptyMSet
applyBackwardStep_mset a (Chg d) = Put (applyBackward d a) EmptyMSet
applyBackwardStep_mset a _ = error "No deletion considered in backward application"

-- This pop_mset function is used to get an element from a multiset
pop_mset :: TypedMSet -> T
pop_mset EmptyMSet = error "pop_mset function cannot be apply to an empty multiset"
pop_mset (Put x xs) = x

