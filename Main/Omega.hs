module Main.Omega 
( ω 
, getDelta
, getSimilarity
, cleanUp
) where

import Main.Value 
import Main.Delta
import Main.Similarity
import Main.Solution

type CutPoint = Double

ω :: T -> T -> CutPoint -> (Delta, Double) 

ω x y p = 
   
   let δ = cleanUp (head_solution (solutions p [(Unknown x y)]))
       σ = fst (bounds_Delta δ)
    in (δ, σ)
--------------------------------------------- 
getDelta :: (Delta, Double) -> Delta
getDelta (x,y) = x

getSimilarity :: (Delta, Double) -> Double
getSimilarity (x,y) = y
 
cleanUp :: Delta -> Delta
cleanUp (Id x) = Id x

--cleanUp (Δ_Prim (Id_F x) _) = Id x
cleanUp (Δ_Prim x y) = Δ_Prim x y

cleanUp (Δ_Pair (Id x) (Id y)) = Id (Pair x y)
cleanUp (Δ_Pair x y) = if (isEveryId (cleanUp x)) && (isEveryId (cleanUp y))
                       then cleanUp (Δ_Pair (cleanUp x) (cleanUp y))
                       else Δ_Pair (cleanUp x) (cleanUp y)

cleanUp (Δ_Union (LL (Id x))) = Id (Union (Lt x))
cleanUp (Δ_Union (LL x)) = if (isEveryId (cleanUp x))
                           then cleanUp (Δ_Union (LL (cleanUp x)))
                           else Δ_Union (LL (cleanUp x))
cleanUp (Δ_Union (RR (Id x))) = Id (Union (Rt x)) 
cleanUp (Δ_Union (RR x)) = if (isEveryId (cleanUp x))
                           then cleanUp (Δ_Union (RR (cleanUp x)))
                           else Δ_Union (RR (cleanUp x))
cleanUp (Δ_Union (LR x y)) = Δ_Union (LR x y)
cleanUp (Δ_Union (RL x y)) = Δ_Union (RL x y)

cleanUp (Δ_Set EmptyStepList) = Id (Set EmptySet)
cleanUp (Δ_Set stepList) = if (isIdStepList (cleanStepList stepList))
                           then Id (Set (toIdSet (cleanStepList stepList)))
                           else Δ_Set (cleanStepList stepList)

cleanUp (Δ_MSet EmptyStepList) = Id (MSet EmptyMSet)
cleanUp (Δ_MSet stepList) = if (isIdStepList (cleanStepList stepList))
                            then Id (MSet (toIdMSet (cleanStepList stepList)))
                            else Δ_MSet (cleanStepList stepList)

cleanUp (Δ_List EmptyStepList) = Id (List EmptyList)
cleanUp (Δ_List stepList) = if (isIdStepList (cleanStepList stepList))
                            then Id (List (toIdList (cleanStepList stepList)))
                            else Δ_List (cleanStepList stepList)

cleanUp (Δ_Map EmptyStepList) = Id (Mapping EmptyMap)
cleanUp (Δ_Map stepList) = if (isIdStepList (cleanStepList stepList))
                           then Id (Mapping (toIdMap (cleanStepList stepList)))
                           else Δ_Map (cleanStepList stepList)

cleanUp (Δ_Rec (Id x)) = Id (Rec x)
cleanUp (Δ_Rec x) = if (isEveryId (cleanUp x))
                    then cleanUp (Δ_Rec (cleanUp x))
                    else Δ_Rec (cleanUp x)

cleanUp (Δ_JSON (Id x)) = Id x
cleanUp (Δ_JSON x) = if (isEveryId (cleanUp x))
                     then cleanUp (Δ_JSON (cleanUp x))
                     else Δ_JSON (cleanUp x)
cleanUp _ = error "other types have not be addressed yet."

cleanStepList :: StepList -> StepList
cleanStepList EmptyStepList = EmptyStepList
cleanStepList (SL x xs) = SL (cleanStep x) (cleanStepList xs)

cleanStep :: Step -> Step
cleanStep (Ins x) = Ins x
cleanStep (Del x) = Del x
cleanStep (Chg (Id x)) = Chg (Id x)
cleanStep (Chg x) = Chg (cleanUp x)

-- This toIdSet function is used to convert a list of steps to a typedset
-- Each step in this list must be (Chg (Id x))
toIdSet :: StepList -> TypedSet
toIdSet EmptyStepList = EmptySet
toIdSet (SL (Chg (Id x)) rest) = Add x (toIdSet rest)

toIdMSet :: StepList -> TypedMSet
toIdMSet EmptyStepList = EmptyMSet
toIdMSet (SL (Chg (Id x)) rest) = Put x (toIdMSet rest)

toIdList :: StepList -> TypedList
toIdList EmptyStepList = EmptyList
toIdList (SL (Chg (Id x)) rest) = Lst x (toIdList rest)

toIdMap :: StepList -> TypedMap
toIdMap EmptyStepList = EmptyMap
toIdMap (SL (Chg (Id (Pair x y))) rest) = Env x y (toIdMap rest)

isEveryId :: Delta -> Bool
isEveryId (Id x) = True
--isEveryId (Δ_Prim (Id_F x) _) = True
isEveryId (Δ_Pair (Id x) (Id y)) = True
isEveryId (Δ_Union (LL (Id x))) = True
isEveryId (Δ_Union (RR (Id x))) = True
isEveryId (Δ_Set stepList) = isIdStepList stepList
isEveryId (Δ_MSet stepList) = isIdStepList stepList
isEveryId (Δ_List stepList) = isIdStepList stepList
isEveryId (Δ_Map stepList) = isIdStepList stepList
isEveryId (Δ_Rec x) = isEveryId x
isEveryId (Δ_JSON x) = isEveryId x
isEveryId _ = False

isIdStepList :: StepList -> Bool
isIdStepList EmptyStepList = True
isIdStepList (SL (Chg (Id x)) rest) = isIdStepList rest
isIdStepList _ = False

