module Main.Delta
( Delta (..)
----------------
, getDeltaJSON
, to_Delta_Rec
, to_Delta_JSON
----------------
, getDeltaRec
, getFst
, getSnd
, getStepList
----------------
, setDelta_to_mapDelta
, Forward (..)
, Backward (..)
, UnionDelta (..)
, Step (..)
------------------
, StepList (..)
, extend_StepList
, combine_StepList
, wrap_DeltaSet
, wrap_DeltaMSet
, wrap_DeltaList
, wrap_DeltaMap
, release_StepList
------------------
, more_to_more
, allchoices
------------------
, more_to_more_mset
, allchoices_mset
------------------
, weight_Delta
, weight_Step
, move_Delta
, move_Step
------------------
, bounds_Delta
, bounds_Step
------------------
, ins_cands
, app 
------------------
, indexed_to_TypedList
) where

import Main.Value
import Main.Eq
import Main.Similarity
 

data Delta = Unknown T T
           | Id T
           | Δ_Prim Forward Backward
           | Δ_Pair Delta Delta
           | Δ_Union UnionDelta
           | Δ_Set StepList
           | Δ_MSet StepList
           | Δ_List StepList
           | Δ_Map StepList
           | Δ_Rec Delta
           | Δ_JSON Delta
            deriving (Eq,Show)

{-
instance Show Delta where
   show (Unknown x y) = "Unknown: " ++ (show x) ++ (show y)
   show (Δ_Prim x y) = (show x) ++ (show y)
   show (Δ_Pair x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
   show (Δ_Union x) = show x
   show (Δ_Set x) = show x
   show (Δ_MSet x) = show x
   show (Δ_List x) = show x
   show (Δ_Map x) = show x
   show (Δ_Rec x) = show x
   show (Δ_JSON x) = show x
-}
---------------------------------------------------------------
getDeltaRec :: Delta -> Delta
getDeltaRec (Δ_Rec d) = d

getDeltaJSON :: Delta -> Delta
getDeltaJSON (Δ_JSON d) = d

getFst :: Delta -> Delta
getFst (Δ_Pair x y) = x

getSnd :: Delta-> Delta
getSnd (Δ_Pair x y) = y

getStepList :: Delta -> StepList
getStepList (Δ_Set sl) = sl
getStepList (Δ_MSet sl) = sl
getStepList (Δ_List sl) = sl
getStepList (Δ_Map sl) = sl
getStepList _ = error "No other Delta constructor contains stepList"

setDelta_to_mapDelta :: [Delta] -> [Delta]
setDelta_to_mapDelta [] = []
setDelta_to_mapDelta ((Δ_Set steps):rest) = (Δ_Map steps):(setDelta_to_mapDelta rest)


to_Delta_Rec :: [Delta] -> [Delta]
to_Delta_Rec [] = []
to_Delta_Rec (d:ds) = (Δ_Rec d):(to_Delta_Rec ds)

to_Delta_JSON :: [Delta] -> [Delta]
to_Delta_JSON [] = []
to_Delta_JSON (d:ds) = (Δ_JSON d):(to_Delta_JSON ds)
------------------------------------------------------------------------------
-- This Forward and Backward data types are used to represent the 
-- forward and backward functions in a directed difference of primitive types.
-- Take the real numbers for example, 
-- if A and B are two real numbers,
-- then the Double value in the constructor F_Double A B the increment is B-A
-- and the value in the constructor B_Double A B the increment is A-B
data Forward = Forward T T deriving Eq
instance Show Forward where
    show (Forward (B _ x) (B _ y)) = "(" ++ (show x) ++ " => " ++ (show y) ++ ")"  
    show (Forward (C _ x) (C _ y)) = "(" ++ (show x) ++ " => " ++ (show y) ++ ")" 
    show (Forward (Nat _ x) (Nat _ y)) = "(" ++ (show x) ++ " => " ++ (show y) ++ ")"
    show (Forward (R _ n1 x) (R _ n2 y)) =
         if (sim_R x y n1) == 0 then "(" ++ (show x) ++ " => " ++ (show y) ++ ")"
         else let increment = y-x
               in if increment >= 0 then (show x) ++ ", ((ƛx.x+" ++ (show increment) ++ ")," 
                  else (show x) ++ ", ((ƛx.x" ++ (show increment) ++ ")," 
    show (Forward (I _ n1 x) (I _ n2 y)) = 
         if (sim_I x y n1) == 0 then "(" ++ (show x) ++ " => " ++ (show y) ++ ")"
         else let increment = y-x
               in if increment >= 0 then (show x) ++ ", ((ƛx.x+" ++ (show increment) ++ "),"
 
                  else (show x) ++ ", ((ƛx.x" ++ (show increment) ++ "),"   
    show (Forward (S _ n1 x) (S _ n2 y)) = "<" ++ (show x) ++ " => " ++ (show y) ++ ">"

data Backward = Backward T T deriving Eq

instance Show Backward where
    show (Backward (B _ x) (B _ y)) = ""--"(" ++ (show x) ++ " <= " ++ (show y) ++ ")"
    show (Backward (C _ x) (C _ y)) = ""--"(" ++ (show x) ++ " <= " ++ (show y) ++ ")"
    show (Backward (Nat _ x) (Nat _ y)) = ""--"(" ++ (show x) ++ " <= " ++ (show y) ++ ")"
    show (Backward (R _ n1 x) (R _ n2 y)) =
          if (sim_R x y n1) == 0 then "" --"(+" ++ (show x) ++ ", -" ++ (show y) ++ ")"
          else let increment = x-y
                in if increment >= 0 then "(ƛx.x+" ++ (show increment) ++ ")), " ++ (show y) 
                   else "(ƛx.x" ++ (show increment) ++ ")), " ++ (show y)

    show (Backward (I _ n1 x) (I _ n2 y)) = 
          if (sim_I x y n1) == 0 then ""--"(" ++ (show x) ++ " <= " ++ (show y) ++ ")"
          else let increment = x-y
                in if increment >= 0 then "(ƛx.x+" ++ (show increment) ++ ")), " ++ (show y)
                   else "(ƛx.x" ++ (show increment) ++ ")), " ++ (show y)
    show (Backward (S _ n1 x) (S _ n2 y)) = ""--"<" ++ (show x) ++ " <= " ++ (show y) ++ ">"
------------------------------------------------------------------------------
data UnionDelta = LL Delta
                | RR Delta
                | LR T T
                | RL T T
                  deriving Eq
instance Show UnionDelta where
    show (LL d) = "ll.( " ++ (show d) ++ " )"
    show (RR d) = "rr.( " ++ (show d) ++ " )"
    show (LR x y) = "lr.( " ++ (show x) ++ ", " ++ (show y) ++ " )"
    show (RL x y) = "rl.( " ++ (show x) ++ ", " ++ (show y) ++ " )"

weight_UnionDelta :: UnionDelta -> Double
weight_UnionDelta (LL d) = weight_Delta d
weight_UnionDelta (RR d) = weight_Delta d
weight_UnionDelta (LR x y) = (weight_Value x) + (weight_Value y)
weight_UnionDelta (RL x y) = (weight_Value x) + (weight_Value y)

move_UnionDelta :: UnionDelta -> Double
move_UnionDelta (LL d) = move_Delta d
move_UnionDelta (RR d) = move_Delta d
move_UnionDelta (LR x y) = (weight_Value x) + (weight_Value y)
move_UnionDelta (RL x y) = (weight_Value x) + (weight_Value y)

bounds_UnionDelta :: UnionDelta -> (Double, Double)
bounds_UnionDelta (LL d) = bounds_Delta d
bounds_UnionDelta (RR d) = bounds_Delta d
bounds_UnionDelta (LR x y) = (0, 0)
bounds_UnionDelta (RL x y) = (0, 0)

------------------------------------------------------------------------------
data Step = Ins T
          | Del T
          | Chg Delta
            deriving Eq
instance Show Step  where
     show (Ins x) = "ins. " ++ (show x)
     show (Del x) = "del. " ++ (show x)
     show (Chg d) = "chg. " ++ (show d)

weight_Step :: Step -> Double
weight_Step (Del x) = weight_Value x
weight_Step (Ins x) = weight_Value x 
weight_Step (Chg d) = weight_Delta d

move_Step :: Step -> Double 
move_Step (Del x) = weight_Value x
move_Step (Ins x) = weight_Value x 
move_Step (Chg d) = move_Delta d

bounds_Step :: Step -> (Double, Double) 
bounds_Step (Ins x) = (0, 0)
bounds_Step (Del x) = (0, 0)
bounds_Step (Chg d) = bounds_Delta d 
-------------------------------------------------------------------------------
data StepList = UnknownRest T T
              | EmptyStepList 
              | SL Step StepList
                deriving Eq
instance Show StepList where
    show sl = "[" ++ (showStepList sl) ++ "]" where
              showStepList (UnknownRest x y) = "unknownRest" 
                                  -- "(unknownRest " ++ (show x) ++ ", " ++ (show y) ++ ")"
              showStepList EmptyStepList = ""
              showStepList (SL x EmptyStepList) = show x
              showStepList (SL x xs) = (show x) ++ ", " ++ (showStepList xs) 

weight_StepList :: StepList -> Double 
weight_StepList (UnknownRest x y) = weight_Delta (Unknown x y)
weight_StepList EmptyStepList = 0
weight_StepList (SL x xs) = (weight_Step x) + (weight_StepList xs)

move_StepList :: StepList -> Double 
move_StepList (UnknownRest x y) = move_Delta (Unknown x y)
move_StepList EmptyStepList = 0
move_StepList (SL x xs) = (move_Step x) + (move_StepList xs)

bounds_StepList :: StepList -> (Double, Double)
bounds_StepList (UnknownRest x y) = bounds_Delta (Unknown x y)
bounds_StepList EmptyStepList = (0, 0)
bounds_StepList (SL x xs) = 
                let lwb_Step_x = fst (bounds_Step x)
                    upb_Step_x = snd (bounds_Step x)
                    lwb_StepList_xs = fst (bounds_StepList xs)
                    upb_StepList_xs = snd (bounds_StepList xs)

                    lwb_StepList = (((lwb_Step_x)*(weight_Step x))+
                                    ((lwb_StepList_xs)*(weight_StepList xs))
                                   )/(weight_StepList (SL x xs))
                    upb_StepList = (((upb_Step_x)*(weight_Step x))+
                                    ((upb_StepList_xs)*(weight_StepList xs))
                                   )/(weight_StepList (SL x xs))
                 in (lwb_StepList, upb_StepList)

-- In this expend_StepList function,
-- lwb_Step step == upb_Step step
extend_StepList :: Step -> [StepList] -> [StepList]
extend_StepList step [] = []
extend_StepList step (x:xs) = (SL step x):(extend_StepList step xs)

combine_StepList :: [Delta] -> StepList -> [StepList]
combine_StepList [] stepList = []
combine_StepList (d:ds) stepList = 
                 (SL (Chg d) stepList):(combine_StepList ds stepList)

wrap_DeltaSet :: [StepList] -> [Delta]
wrap_DeltaSet [] = []
wrap_DeltaSet (x:xs) = (Δ_Set x):(wrap_DeltaSet xs)

wrap_DeltaMSet :: [StepList] -> [Delta]
wrap_DeltaMSet [] = []
wrap_DeltaMSet (x:xs) = (Δ_MSet x):(wrap_DeltaMSet xs)


wrap_DeltaList :: [StepList] -> [Delta]
wrap_DeltaList [] = []
wrap_DeltaList (x:xs) = (Δ_List x):(wrap_DeltaList xs)

wrap_DeltaMap :: [StepList] -> [Delta]
wrap_DeltaMap [] = []
wrap_DeltaMap (x:xs) = (Δ_Map x):(wrap_DeltaMap xs)

release_StepList :: [Delta] -> [StepList]
release_StepList [] = []
release_StepList ((Δ_Set xs):rest) = xs:(release_StepList rest)
release_StepList ((Δ_MSet xs):rest) = xs:(release_StepList rest)
release_StepList ((Δ_List xs):rest) = xs:(release_StepList rest)
release_StepList ((Δ_Map xs):rest) = xs:(release_StepList rest)
   
-----------------------------------------------------------------------------------------------
-- These weight functions are used to calculate the weight of deltas based on the weight of values. And the result value are the foundation of lower bound and upper bound of the corresponding delta.
weight_Delta :: Delta -> Double 
weight_Delta (Unknown x y) = (weight_Value x) + (weight_Value y)

weight_Delta (Id x) = 2*(weight_Value x)

weight_Delta (Δ_Prim (Forward (B w1 x) (B w2 y)) _) = w1 + w2
weight_Delta (Δ_Prim (Forward (C w1 x) (C w2 y)) _) = w1 + w2
weight_Delta (Δ_Prim (Forward (Nat w1 x) (Nat w2 y)) _) = w1 + w2

weight_Delta (Δ_Prim (Forward (R w1 n1 x) (R w2 n2 y)) _) = w1 + w2 
 
weight_Delta (Δ_Prim (Forward (I w1 n1 x) (I w2 n2 y)) _) = w1 + w2
 
weight_Delta (Δ_Prim (Forward (S w1 n1 x) (S w2 n2 y)) _) = w1 + w2

---------------------------------------------------------------------------------
weight_Delta (Δ_Prim (Forward x y) _) = (weight_Value x) + (weight_Value y)
--------------------------------------------------------------------------------- 
weight_Delta (Δ_Pair x y) = (weight_Delta x) + (weight_Delta y)
weight_Delta (Δ_Union x) = weight_UnionDelta x
weight_Delta (Δ_Set xs) = weight_StepList xs 
weight_Delta (Δ_MSet xs) = weight_StepList xs 
weight_Delta (Δ_List xs) = weight_StepList xs 
weight_Delta (Δ_Map xs) = weight_StepList xs

weight_Delta (Δ_Rec x) = weight_Delta x
weight_Delta (Δ_JSON x) = weight_Delta x 

-----------------------------------------------------------------------------------------------
-- These move functions are used to determine the workload of a delta.
-- In the ins_cand function in Solution module,
-- this value is expected as small as possible.
move_Delta :: Delta -> Double 
move_Delta (Unknown x y) = 0 

move_Delta (Id x) = 0

move_Delta (Δ_Prim (Forward (B w1 t1) (B w2 t2)) _) = w1 + w2
move_Delta (Δ_Prim (Forward (C w1 t1) (C w2 t2)) _) = w1 + w2
move_Delta (Δ_Prim (Forward (Nat w1 t1) (Nat w2 t2)) _) = w1 + w2

move_Delta (Δ_Prim (Forward (R w1 n1 x) (R w2 n2 y)) _ ) = 
           if (not (eqDouble n1 n2)) then error "Wrong in Delta move_Delta function R type"
           else if (sim_R x y n1) == 0 
                then (w1 + w2)
                else 0

move_Delta (Δ_Prim (Forward (I w1 n1 x) (I w2 n2 y)) _ ) =
           if (n2 /= n2) then error "Wrong in Delta move_Delta function I type"
           else if (sim_I x y n1) == 0 
                then (w1 + w2)
                else 0

move_Delta (Δ_Prim (Forward (S w1 n1 x) (S w2 n2 y)) _ ) = 
           if (n1 /= n2) then error "Wrong in Delta move_Delta function S type"
           else if (sim_S x y n1) == 0 
                then (w1 + w2)
                else 0
-----------------------------------------------------------------------------
move_Delta (Δ_Prim (Forward x y) _) = (weight_Value x) + (weight_Value y)
-----------------------------------------------------------------------------
move_Delta (Δ_Pair x y) = (move_Delta x) + (move_Delta y)
move_Delta (Δ_Union x) = (move_UnionDelta x)
move_Delta (Δ_Set xs) = move_StepList xs
move_Delta (Δ_MSet xs) = move_StepList xs
move_Delta (Δ_List xs) = move_StepList xs
move_Delta (Δ_Map xs) = move_StepList xs

move_Delta (Δ_Rec x) = move_Delta x
move_Delta (Δ_JSON x) = move_Delta x 

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
bounds_Delta :: Delta -> (Double, Double)

bounds_Delta (Unknown (Set EmptySet) (Set EmptySet)) = (1, 1)
bounds_Delta (Unknown (List EmptyList) (List EmptyList)) = (1, 1)
bounds_Delta (Unknown (Mapping EmptyMap) (Mapping EmptyMap)) = (1, 1)
bounds_Delta (Unknown x y) = (0, 1)

bounds_Delta (Id x) = (1, 1)

bounds_Delta (Δ_Prim (Forward (B _ x) (B _ y)) _) = (0, 0)
bounds_Delta (Δ_Prim (Forward (C _ x) (C _ y)) _) = (0, 0)
bounds_Delta (Δ_Prim (Forward (Nat _ x) (Nat _ y)) _) = (0, 0)

bounds_Delta (Δ_Prim (Forward (R _ n1 x) (R _ n2 y)) _) =
          if (not (eqDouble n1 n2)) 
          then error "bounds_Delta: These two real numbers have different accuracy" 
          else ((sim_R x y n1), (sim_R x y n1))

bounds_Delta (Δ_Prim (Forward (I _ n1 x) (I _ n2 y)) _) = 
          if (n1 /= n2) 
          then error "bounds_Delta: These two integers have different accuracy" 
          else ((sim_I x y n1), (sim_I x y n1))

bounds_Delta (Δ_Prim (Forward (S _ n1 x) (S _ n2 y)) _) = 
          if (n1 /= n2) 
          then error "bounds_Delta: These two strings have different accuracy"
          else ((sim_S x y n1), (sim_S x y n1))
---------------------------------------
bounds_Delta (Δ_Prim _ _) = (0, 0)
---------------------------------------
bounds_Delta (Δ_Pair x y) = 
          let lwb_Delta_x = fst (bounds_Delta x)
              upb_Delta_x = snd (bounds_Delta x)
              lwb_Delta_y = fst (bounds_Delta y)
              upb_Delta_y = snd (bounds_Delta y)

              lwb_Delta_Pair = (((lwb_Delta_x)*(weight_Delta x))+
                                ((lwb_Delta_y)*(weight_Delta y))
                               )/((weight_Delta x)+(weight_Delta y))
              upb_Delta_Pair = (((upb_Delta_x)*(weight_Delta x))+
                               ((upb_Delta_y)*(weight_Delta y))
                               )/((weight_Delta x)+(weight_Delta y))
           in (lwb_Delta_Pair, upb_Delta_Pair)

bounds_Delta (Δ_Union x) = bounds_UnionDelta x

bounds_Delta (Δ_Set xs) = bounds_StepList xs

bounds_Delta (Δ_MSet xs) = bounds_StepList xs

bounds_Delta (Δ_List xs) = bounds_StepList xs
  
bounds_Delta (Δ_Map xs) = bounds_StepList xs

bounds_Delta (Δ_Rec x) = bounds_Delta x

bounds_Delta (Δ_JSON x) = bounds_Delta x 
          
--------------------------------------------------
--Operations for TypedSet
-- By using these one_to_more and more_to_more functions,
-- we can obtain every pairs between elements in two TypedSets.

-- Then these pairs need to be sorted by the lwb and upb of them 

one_to_more :: T -> TypedSet -> [(T, T)]
one_to_more x EmptySet = []
one_to_more x (Add y ys) = if (not (isJSON x))
                           then ((x, y):(one_to_more x ys))
                           else (if (isSameJSON x y)
                                 then ((x, y):(one_to_more x ys))
                                 else one_to_more x ys
                                )

more_to_more :: TypedSet -> TypedSet -> [(T, T)]
more_to_more _ EmptySet = []
more_to_more EmptySet _ = [] 
more_to_more (Add x xs) (Add y ys) = 
             
             app (one_to_more x (Add y ys))
                 (more_to_more xs (Add y ys))

-- This choice function is used to get the corresponding rest unknown from the given unknown.
-- This function is particular used to identify the rest part of two typed set
-- For two non-empty typed set, the result is one option of Δ_Set
choice :: (T, T) -> TypedSet -> TypedSet -> Delta
choice _ EmptySet _ = error "The original set is empty."
choice _ _ EmptySet = error "The targert set is empty."
choice (x, y) a b = 
            
       Δ_Set (SL (Chg (Unknown x y))
                     (UnknownRest (Set (getRest a x)) (Set (getRest b y)))
                 )

allchoices :: [(T,T)] -> TypedSet -> TypedSet -> [Delta]
allchoices [] _ _ = []
allchoices ((x,y):rest) a b = 

           (choice (x,y) a b):(allchoices rest a b)
--------------------------------------------------
--Operations for TypedMSet
-- By using these one_to_more and more_to_more functions,
-- we can obtain every pairs between elements in two typed multisets.

-- Then these pairs need to be sorted by the lwb and upb of them 

one_to_more_mset :: T -> TypedMSet -> [(T,T)]
one_to_more_mset x EmptyMSet = []
one_to_more_mset x (Put y ys) = (x, y):(one_to_more_mset x ys)

more_to_more_mset :: TypedMSet -> TypedMSet -> [(T,T)]
more_to_more_mset _ EmptyMSet = []
more_to_more_mset EmptyMSet _ = [] 
more_to_more_mset (Put x xs) (Put y ys) = 
             
             app (one_to_more_mset x (Put y ys))
                 (more_to_more_mset xs (Put y ys))

-- This choice function is used to get the corresponding rest unknown from the given unknown.
-- This function is particular used to identify the rest part of two typed set
-- For two non-empty typed set, the result is one option of Δ_Set
choice_mset :: (T,T) -> TypedMSet -> TypedMSet -> Delta
choice_mset _ EmptyMSet _ = error "The original multi set is empty."
choice_mset _ _ EmptyMSet = error "The targert multi set is empty."
choice_mset (x, y) a b = 
            
        Δ_MSet (SL (Chg (Unknown x y))
                       (UnknownRest (MSet (getMRest a x)) (MSet (getMRest b y)))
                   )

allchoices_mset :: [(T,T)] -> TypedMSet -> TypedMSet -> [Delta]
allchoices_mset [] _ _ = []
allchoices_mset ((x, y):rest) a b = 

           (choice_mset (x, y) a b):(allchoices_mset rest a b)

----------------------------------------------------------------------------------------------
--------------------------------------------------
-- These two insertion functions are used to sort the elements in a list of Delta

-- 1. This ins_cands function is used to insert a list of Delta into a sorted list of Delta
-- Firstly, the first list is sorted.
-- The approach taken is insert each element in the second list one by one. 
ins_cands :: [Delta] -> [Delta] -> [Delta]
ins_cands [] [] = []
ins_cands [] (y:ys) = ins_cands [y] ys
ins_cands xs [] = xs 
ins_cands xs (y:ys) = ins_cands (ins_cand xs y) ys

-- 2. This ins_cand function is used to insert a Delta into a sorted list of Delta.
-- The essential part is the original list is sorted.
-- The approach taken to insert this new_delta is to compare the lower bound and the upper bound of each element in the sorted list and the corresponding values of lower bound and the upper bound of the new_delta.

ins_cand :: [Delta] -> Delta -> [Delta]
ins_cand [] x = [x]
ins_cand (current_delta:cands) new_delta =
                   
    let lwb_cur = fst (bounds_Delta current_delta)
        upb_cur = snd (bounds_Delta current_delta)
        lwb_new = fst (bounds_Delta new_delta)
        upb_new = snd (bounds_Delta new_delta)

        move_cur = move_Delta current_delta
        move_new = move_Delta new_delta
-- If the lower bound of the new_delta is greater than the lower bound of the current_delta,
-- This new_delta will be inserted in front of the current_delta
     in if (lwb_new > lwb_cur)
        then new_delta:(current_delta:cands)
-- Else if the lower bound of this new_delta equals to the lower bound of the current_delta,
        else ( if (lwb_new == lwb_cur)
-- the weight of the movement of these two deltas needs to be considered.
-- Under this case, the smaller movement weight is prefered.
               then ( if (move_new < move_cur)
                      then new_delta:(current_delta:cands)
-- When the weights of movement of these two deltas are identical,
                      else ( if (move_new == move_cur)
-- and if the upper bound of new_delta is greater than the upper bound of current_delta
-- in the other word, if the estimate similarity of new_delta is greater than 
-- the estimate similarity of the current_delta, the new_delta is inserted
                             then (if (upb_new > upb_cur)
                                   then new_delta:(current_delta:cands)
                                   else current_delta:(ins_cand cands new_delta)
                                  )
                             else current_delta:(ins_cand cands new_delta)
                           ) 
                    )
-- When the lower bound of new_delta is less than the lower bound of current_delta
-- and when the upper bound of the new_delta is less than the lower bound of current_delta,
-- this branch will be disregarded.
               else ( if (upb_new < lwb_cur)
                      then current_delta:cands
                      else current_delta:(ins_cand cands new_delta)
                    )
             )

----------------------------------------------------------------------------------------------
-- 1. This app function is used to connect two lists.
-- The approach taken is append the element in the second list
-- at the end of the first list.
app :: [a] -> [a] -> [a]
app [] ys = ys
app (x:xs) ys = x:(app xs ys)

-- 2. This append function is used to append one element at the end of a list
append :: [a] -> a -> [a]
append [] y = [y]
append (x:xs) y = x:(append xs y)
---------------------------------------------------------------
-- This indexed_to_TypedList function is used to convert the [(T, Step)]
-- into a typed list over type (Pair (I, T)).
-- When this function apply to the result list of orig_StepList
-- The indexed original list will be returned
-- When this function apply to the result list of targ_StepList
-- The indexed target list will be returned
indexed_to_TypedList :: [(T, Step)] -> TypedList
indexed_to_TypedList [] = EmptyList 
indexed_to_TypedList ((x,step):xs) = Lst (Pair (I 0 1 ((length xs) +1)) x) (indexed_to_TypedList xs) 

