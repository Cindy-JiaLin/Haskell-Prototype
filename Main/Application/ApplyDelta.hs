module Main.Application.ApplyDelta
( ApplyDelta (..)
, getASL
, getSingleFst
, getSingleSnd
---------------------
, ApplyStep (..)
, getApplyStep
------------------
, ApplyStepList (..)
, getHead
, extend_ApplyStepList
, combine_ApplyStepList
, wrap_ApplyDelta
, release_ApplyStepList
------------------
, tlist_tslist
, allways
------------------
------------------
, weight_ApplyDelta
, weight_ApplyStep
------------------
, bounds_ApplyDelta
, bounds_ApplyStep
------------------
, ins_Applycands
) where

import Main.Value
import Main.Delta
import Main.Eq
import Main.Similarity
import Main.Omega
 
data ApplyDelta = ApplyUnknown [T] [(T,Step)]
                | ApplySingle T (T,Step)
                | ApplyDelta ApplyStepList
                  deriving Eq
instance Show ApplyDelta where
    show (ApplyUnknown xs ys) = (show xs) ++ ", " ++ (show ys)
    show (ApplySingle x (y,step)) = -- "(" ++ (show x) ++ " -> " ++ (show y) ++ ")"
                                    "ApplySingle: (" ++ (show x) ++ " -> " ++(show y) ++ "STEP:" ++ (show step) ++ ")"

    show (ApplyDelta asl) = "Matching:" ++ (show asl)

-----------------------------------------------------------------------------------
-- get the ApplyStepList in the ApplyDelta constructor
getASL :: ApplyDelta -> ApplyStepList
getASL (ApplyDelta asl) = asl

getSingleFst :: ApplyDelta -> T
getSingleFst (ApplySingle x _) = x

getSingleSnd :: ApplyDelta -> T
getSingleSnd (ApplySingle _ (y, _)) = y
---------------------------------------------------------------------------------------------
-- These weight functions are used to calculate the weight of Applydeltas based on the weight of values. And the result value are the foundation of lower bound and upper bound of the corresponding delta.
weight_ApplyDelta :: ApplyDelta -> Double 
weight_ApplyDelta (ApplyUnknown [] []) = 0
weight_ApplyDelta (ApplyUnknown (x:xs) []) = 
                  (weight_Value x) + (weight_ApplyDelta (ApplyUnknown xs []))
weight_ApplyDelta (ApplyUnknown [] ((y,step):ys)) =
                  (weight_Value y) + (weight_ApplyDelta (ApplyUnknown [] ys))
weight_ApplyDelta (ApplyUnknown (x:xs) ((y,step):ys)) =
                  (weight_Value x) + (weight_Value y) 
                  + (weight_ApplyDelta (ApplyUnknown xs ys))

weight_ApplyDelta (ApplySingle x (y,step)) = (weight_Value x) + (weight_Value y) 
weight_ApplyDelta (ApplyDelta xs) = weight_ApplyStepList xs 

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
bounds_ApplyDelta :: ApplyDelta -> (Double, Double)

bounds_ApplyDelta (ApplyUnknown [] []) = (1, 1)

bounds_ApplyDelta (ApplyUnknown xs ys) = (0, 1)

bounds_ApplyDelta (ApplySingle x (y,step)) = let σ = getSimilarity (ω x y 1)
                                              in (σ, σ) 

bounds_ApplyDelta (ApplyDelta xs) = bounds_ApplyStepList xs

------------------------------------------------------------------------------
------------------------------------------------------------------------------
data ApplyStep = ApplyIns (T,Step)
               | ApplyDel T
               | ApplyChg ApplyDelta
            deriving Eq
instance Show ApplyStep where
    show (ApplyIns (y,step)) = "left change: " ++ (show step) 
    show (ApplyDel x) = "left element: " ++ (show x)
    show (ApplyChg d) = "applyChg: " ++ (show d)

getApplyStep :: ApplyStep -> ApplyDelta
getApplyStep (ApplyChg d) = d
-------------------------------------------------------------------------------
weight_ApplyStep :: ApplyStep -> Double 
weight_ApplyStep (ApplyDel x) = weight_Value x
weight_ApplyStep (ApplyIns (x,_)) = weight_Value x 
weight_ApplyStep (ApplyChg d) = weight_ApplyDelta d

bounds_ApplyStep :: ApplyStep -> (Double, Double) 
bounds_ApplyStep (ApplyIns x) = (0, 0)
bounds_ApplyStep (ApplyDel x) = (0, 0)
bounds_ApplyStep (ApplyChg d) = bounds_ApplyDelta d 
-------------------------------------------------------------------------------
data ApplyStepList = ApplyUnknownRest [T] [(T,Step)]
                   | ApplyEmptyStepList 
                   | ApplySL ApplyStep ApplyStepList
                     deriving Eq
instance Show ApplyStepList where
    show asl = "[" ++ (showApplyStepList asl) ++ "]" where
              showApplyStepList (ApplyUnknownRest x y) = "UnknownRest" 
                           --"(ApplyUnknownRest " ++ (show x) ++ ", " ++ (show y) ++ ")"
              showApplyStepList ApplyEmptyStepList = ""
              showApplyStepList (ApplySL x ApplyEmptyStepList) = show x
              showApplyStepList (ApplySL x xs) = (show x) ++ ", " ++ (showApplyStepList xs)

getHead :: ApplyStepList -> ApplyStep
getHead (ApplySL x xs) = x 
-------------------------------------------------------------------------------
weight_ApplyStepList :: ApplyStepList -> Double 
weight_ApplyStepList (ApplyUnknownRest xs ys) = weight_ApplyDelta (ApplyUnknown xs ys)
weight_ApplyStepList ApplyEmptyStepList = 0
weight_ApplyStepList (ApplySL x xs) = (weight_ApplyStep x) + (weight_ApplyStepList xs)

bounds_ApplyStepList :: ApplyStepList -> (Double, Double)
bounds_ApplyStepList (ApplyUnknownRest x y) = bounds_ApplyDelta (ApplyUnknown x y)
bounds_ApplyStepList ApplyEmptyStepList = (0, 0) 
bounds_ApplyStepList (ApplySL x xs) = 
             let lwb_ApplyStep_x = fst (bounds_ApplyStep x)
                 upb_ApplyStep_x = snd (bounds_ApplyStep x)
                 lwb_ApplyStepList_xs = fst (bounds_ApplyStepList xs)
                 upb_ApplyStepList_xs = snd (bounds_ApplyStepList xs)

                 lwb_ApplyStepList = (((lwb_ApplyStep_x)*(weight_ApplyStep x))+
                                      ((lwb_ApplyStepList_xs)*(weight_ApplyStepList xs))
                                     )/(weight_ApplyStepList (ApplySL x xs))

                 upb_ApplyStepList = (((upb_ApplyStep_x)*(weight_ApplyStep x))+
                                      ((upb_ApplyStepList_xs)*(weight_ApplyStepList xs))
                                     )/(weight_ApplyStepList (ApplySL x xs))
              in (lwb_ApplyStepList, upb_ApplyStepList)

-- In this expend_ApplyStepList function,
-- lwb_Step step == upb_Step step
extend_ApplyStepList :: ApplyStep -> [ApplyStepList] -> [ApplyStepList]
extend_ApplyStepList step [] = []
extend_ApplyStepList step (x:xs) = (ApplySL step x):(extend_ApplyStepList step xs)

combine_ApplyStepList :: [ApplyDelta] -> ApplyStepList -> [ApplyStepList]
combine_ApplyStepList [] stepList = []
combine_ApplyStepList (d:ds) stepList = 
                      (ApplySL (ApplyChg d) stepList):(combine_ApplyStepList ds stepList)

wrap_ApplyDelta :: [ApplyStepList] -> [ApplyDelta]
wrap_ApplyDelta [] = []
wrap_ApplyDelta (x:xs) = (ApplyDelta x):(wrap_ApplyDelta xs)

release_ApplyStepList :: [ApplyDelta] -> [ApplyStepList]
release_ApplyStepList [] = []
release_ApplyStepList ((ApplyDelta xs):rest) = xs:(release_ApplyStepList rest)
          
--------------------------------------------------
-- These t_tslist and tlist_tslist functions are used to 
-- combine every two element from C and D
t_tslist :: T -> [(T,Step)] -> [(T, (T, Step))]
t_tslist x [] = []
t_tslist x ((y,step):ys) = (x, (y,step)):(t_tslist x ys)

tlist_tslist :: [T] -> [(T,Step)] -> [(T, (T, Step))]
tlist_tslist _ [] = []
tlist_tslist [] _ = [] 
tlist_tslist (x:xs) ys = 
             
             app (t_tslist x ys)
                 (tlist_tslist xs ys)

-- These two functions getLeft_tlist and getLeft_tslist are used to 
-- get the rest list of an element.
getLeft_tlist :: [T] -> T -> [T]
getLeft_tlist [] _ = []
getLeft_tlist (x:xs) a = 
              if eq x a then xs
              else x:(getLeft_tlist xs a)

getLeft_tslist :: [(T,Step)] -> (T,Step) -> [(T,Step)]
getLeft_tslist [] _ = []
getLeft_tslist ((y,step):ys) (a,s) = 
               if (eq y a) && (step==s)
               then ys
               else (y,step):(getLeft_tslist ys (a,s))               

oneway :: (T, (T, Step)) -> [T] -> [(T,Step)] -> ApplyDelta
oneway _ [] _ = error "The original set is empty."
oneway _ _ [] = error "The targert set is empty."
oneway (x, y) xs ys = 
          
       ApplyDelta (ApplySL (ApplyChg (ApplySingle x y))
                           (ApplyUnknownRest (getLeft_tlist xs x) (getLeft_tslist ys y))
                  )

allways :: [(T, (T, Step))] -> [T] -> [(T,Step)] -> [ApplyDelta]
allways [] _ _ = []
allways (d:ds) xs ys = 

        (oneway d xs ys):(allways ds xs ys)
----------------------------------------------------------------------------------------------
--------------------------------------------------
-- These two insertion functions are used to sort the elements in a list of ApplyDelta

-- 1. This ins_Applycands function is used to insert a list of ApplyDelta into 
-- a sorted list of ApplyDelta
-- Firstly, the first list is sorted.
-- The approach taken is insert each element in the second list one by one. 
ins_Applycands :: [ApplyDelta] -> [ApplyDelta] -> [ApplyDelta]
ins_Applycands [] [] = []
ins_Applycands [] (y:ys) = ins_Applycands [y] ys
ins_Applycands xs [] = xs 
ins_Applycands xs (y:ys) = ins_Applycands (ins_Applycand xs y) ys 

-- 2. This ins_Applycand function is used to insert a ApplyDelta into a sorted list of ApplyDelta.
-- The essential part is the original list is sorted.
-- The approach taken to insert this new_Applydelta is to compare the lower bound and the upper bound of each element in the sorted list and the corresponding values of lower bound and the upper bound of the new_Applydelta.

ins_Applycand :: [ApplyDelta] -> ApplyDelta -> [ApplyDelta]
ins_Applycand [] x = [x]
ins_Applycand (current_Applydelta:cands) new_Applydelta =
                   
    let lwb_cur = fst (bounds_ApplyDelta current_Applydelta)
        upb_cur = snd (bounds_ApplyDelta current_Applydelta)
        lwb_new = fst (bounds_ApplyDelta new_Applydelta)
        upb_new = snd (bounds_ApplyDelta new_Applydelta)

-- If the lower bound of the new_Applydelta is greater than the lower bound of the current_Applydelta,
-- This new_Applydelta will be inserted in front of the current_Applydelta
     in if (lwb_new > lwb_cur)
        then new_Applydelta:(current_Applydelta:cands)
-- Else if the lower bound of this new_Applydelta equals to the lower bound of the current_Applydelta,
{-
        else ( if (lwb_new == lwb_cur)
-- and if the upper bound of new_delta is greater than the upper bound of current_delta
-- in the other word, if the estimate similarity of new_delta is greater than 
-- the estimate similarity of the current_delta, the new_delta is inserted
               then (if (upb_new > upb_cur)
                     then new_Applydelta:(current_Applydelta:cands)
                     else current_Applydelta:(ins_Applycand cands new_Applydelta)
                    )
-- When the lower bound of new_delta is less than the lower bound of current_delta
-- and when the upper bound of the new_delta is less than the lower bound of current_delta,
-- this branch will be disregarded.
               else ( if (upb_new < lwb_cur)
                      then current_Applydelta:cands
-}
                      else current_Applydelta:(ins_Applycand cands new_Applydelta)
                   -- )
            -- )


