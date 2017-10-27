module Main.Application.Targ
( targ_StepList
, targ_Delta
) where

import Main.Value 
import Main.Delta

import Main.Application.Orig

-- This targ_StepList function is used to build a list of pairs.
-- Each pair in this list contains a element and its corresponding edit operation.
-- This list of pairs is the target collection object (a set, mset, list or mapping)
-- Since their directed difference depends on a steplist
targ_StepList :: StepList -> [(T, Step)]
targ_StepList (UnknownRest x y) =
               []
targ_StepList EmptyStepList =
               []
targ_StepList (SL (Del x) stepList) =
               targ_StepList stepList
-- In particular, when the edit operation is an insertion of an element
-- this element belongs to the target collection object.
-- Because of the this object was inserted from the target object. 
targ_StepList (SL (Ins y) stepList) =
               (y, (Ins y)):(targ_StepList stepList)
targ_StepList (SL (Chg d) stepList) =
               ((targ_Delta d), (Chg d)):(targ_StepList stepList)
------------------------------------------------------------------------------------
-- This targ_Delta function is used to get the target object from a given delta 
targ_Delta :: Delta -> T
targ_Delta (Id x) = x
targ_Delta (Δ_Prim (Forward x y) _) = y
targ_Delta (Δ_Pair x y) = Pair (targ_Delta x) (targ_Delta y)
targ_Delta (Δ_Set xs) = Set (to_TypedSet (targ_StepList xs))
targ_Delta (Δ_MSet xs) = MSet (to_TypedMSet (targ_StepList xs))
targ_Delta (Δ_List xs) = List (to_TypedList (targ_StepList xs))
targ_Delta (Δ_Map xs) = Mapping (to_TypedMap (targ_StepList xs))
targ_Delta (Δ_Rec d) = Rec (targ_Delta d)

targ_Delta (Δ_JSON (Δ_Prim (Forward x (B _ y)) _)) = JSON (JBool y)
targ_Delta (Δ_JSON (Δ_Prim (Forward x (S _ _ y)) _)) = JSON (JString y)
targ_Delta (Δ_JSON (Δ_Prim (Forward x (R _ _ y)) _)) = JSON (JNumber y)
targ_Delta (Δ_JSON (Δ_List xs)) = 
           JSON (JArray (to_ljv (getTypedList (targ_Delta (Δ_List xs)))))
targ_Delta (Δ_JSON (Δ_Set xs)) =
           JSON (JObject (set_to_ljo (getTypedSet (targ_Delta (Δ_Set xs))))) 
------------------------------------------------------------------------------------
