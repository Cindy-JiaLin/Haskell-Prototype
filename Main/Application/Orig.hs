module Main.Application.Orig
( orig_StepList
, orig_Delta
, to_TypedSet
, to_TypedMSet
, to_TypedList
, to_TypedMap
) where

import Main.Value 
import Main.Delta

-- This orig_StepList is used to extract the element in the original object.
-- This original object maybe set, multiset, list or mapping.
-- Since their directed difference was defined depends on the steplist.
-- By applying this function to a steplist, the element in the original collection object
-- and its corresponding edit operation will be obtained.
-- Then a list of such pairs are returned.
orig_StepList :: StepList -> [(T, Step)]
orig_StepList (UnknownRest x y) =
             []
orig_StepList EmptyStepList =
             []
-- However, the original collection only contains the deleted element and the changed element
-- the element was inserted belongs to the target collection.
orig_StepList (SL (Del x) stepList) =
              (x, (Del x)):(orig_StepList stepList)
orig_StepList (SL (Ins y) stepList) =
              orig_StepList stepList
orig_StepList (SL (Chg d) stepList) =
              ((orig_Delta d), (Chg d)):(orig_StepList stepList)

---------------------------------------------------------------------------------------------- 
-- This to_TypedSet function is used to convert list of pairs to a set
-- The pair in this lists contains the element in the result set and 
-- its corresponding edit operation
to_TypedSet :: [(T, Step)] -> TypedSet
to_TypedSet [] = EmptySet
to_TypedSet ((x,s):rest) = Add x (to_TypedSet rest)
-- here Add is for JSON value, 
-- since a JObject [(String, JValue)] in which a JValue can be JString or any other
-- JValue, when add (str, Jstr) and (str, JObj), the system will view them as different
-- Type of pairs, then cannot be added into one typedSet.

to_TypedMSet :: [(T, Step)] -> TypedMSet
to_TypedMSet [] = EmptyMSet
to_TypedMSet ((x,s):rest) = put x (to_TypedMSet rest)

to_TypedList :: [(T, Step)] -> TypedList
to_TypedList [] = EmptyList
to_TypedList ((x,s):rest) = ins x (to_TypedList rest)

to_TypedMap :: [(T, Step)] -> TypedMap
to_TypedMap [] = EmptyMap
to_TypedMap ((x,s):rest) = extend x (to_TypedMap rest)
----------------------------------------------------------------------------------------------
-- This orig_Delta function is used to get the original object from a delta
orig_Delta :: Delta -> T
orig_Delta (Id x) = x
orig_Delta (Δ_Prim (Forward x y) _) = x
orig_Delta (Δ_Pair x y) = Pair (orig_Delta x) (orig_Delta y)
orig_Delta (Δ_Set xs) = Set (to_TypedSet (orig_StepList xs))
orig_Delta (Δ_MSet xs) = MSet (to_TypedMSet (orig_StepList xs))
orig_Delta (Δ_List xs) = List (to_TypedList (orig_StepList xs))
orig_Delta (Δ_Map xs) = Mapping (to_TypedMap (orig_StepList xs))
orig_Delta (Δ_Rec d) = Rec (orig_Delta d)

orig_Delta (Δ_JSON (Δ_Prim (Forward (B _ x) y) _)) = JSON (JBool x)
orig_Delta (Δ_JSON (Δ_Prim (Forward (S _ _ x) y) _)) = JSON (JString x)
orig_Delta (Δ_JSON (Δ_Prim (Forward (R _ _ x) y) _)) = JSON (JNumber x)
orig_Delta (Δ_JSON (Δ_List xs)) = 
           JSON (JArray (to_ljv (getTypedList (orig_Delta (Δ_List xs)))))
orig_Delta (Δ_JSON (Δ_Set xs)) =
           JSON (JObject (set_to_ljo (getTypedSet (orig_Delta (Δ_Set xs))))) 
---------------------------------------------------------------------------------------------


