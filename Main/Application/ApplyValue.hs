module Main.Application.ApplyValue 
( convert_Set
, convert_MSet
, convert_List
, convert_Mapping
, sortedSet
, de_indexed
) where

import Main.Value

-- These convert_ functions are used to convert a set, multiset, list or mapping to 
-- a list contains their elements.
-- The result of this function will be used to apply to a list of pairs [(T,Step)]
-- obtained from functions Orig.orig_StepList and Targ.targ_StepList 
convert_Set :: TypedSet -> [T]
convert_Set EmptySet = []
convert_Set (Add x xs) = 
            x:(convert_Set xs)

convert_MSet :: TypedMSet -> [T]
convert_MSet EmptyMSet = []
convert_MSet (Put x xs) = 
             x:(convert_MSet xs)

convert_List :: TypedList -> [T]
convert_List EmptyList = []
convert_List (Lst x xs) = 
             x:(convert_List xs)

convert_Mapping :: TypedMap -> [T]
convert_Mapping EmptyMap = []
convert_Mapping (Env x y rest) = 
                (Pair x y):(convert_Mapping rest)

---------------------------------------------------------------------------------------------
-- This sortedSet function is used to sort the element in the set theoretical model of a list
-- Each element in this set is a pair whose type is (Pair (I 0 1 i) T)
sortedSet :: TypedSet -> TypedSet
sortedSet EmptySet = EmptySet
sortedSet (Add x xs) = let smaller = sortedSet (smallerSet x xs)
                           bigger = sortedSet (biggerSet x xs)
                           xSet = Add x EmptySet
                           tail = union_TypedSet bigger xSet
                        in union_TypedSet  tail smaller 

smallerSet :: T -> TypedSet -> TypedSet
smallerSet (Pair (I 0 1 i) x) EmptySet = EmptySet
smallerSet (Pair (I 0 1 i) x) (Add (Pair (I 0 1 j) y) ys) =
           
           if j < i then (Add (Pair (I 0 1 j) y) (smallerSet (Pair (I 0 1 i) x) ys))
           else smallerSet (Pair (I 0 1 i) x) ys

biggerSet :: T -> TypedSet -> TypedSet
biggerSet (Pair (I 0 1 i) x) EmptySet = EmptySet
biggerSet (Pair (I 0 1 i) x) (Add (Pair (I 0 1 j) y) ys) =
           
           if j > i then (Add (Pair (I 0 1 j) y) (biggerSet (Pair (I 0 1 i) x) ys))
           else biggerSet (Pair (I 0 1 i) x) ys

--------------------------------------------------------------------------------------------
-- This de_indexed function is used to recover the set theoretical model of a list to a list.
-- This function will be used to implement 
-- the applyForward and applyBackward functions for lists.
de_indexed :: TypedSet -> TypedList
de_indexed EmptySet = EmptyList
de_indexed (Add (Pair i x) xs) = ins x (de_indexed xs)                    
                     
