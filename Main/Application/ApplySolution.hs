module Main.Application.ApplySolution 
( applySolutions
, polish
, polish_ApplyStepList
) where

import Main.Value
import Main.Delta
import Main.Solution

import Main.Application.ApplyDelta
------------------------------------------------------------------------------------------------------------------------------------------
polish :: ApplyDelta -> [ApplyDelta]

polish (ApplyUnknown [] []) =
       
       [(ApplyDelta ApplyEmptyStepList)]

polish (ApplyUnknown (x:xs) []) =

       let [(ApplyDelta stepList)] = polish (ApplyUnknown xs [])
        in [(ApplyDelta (ApplySL (ApplyDel x) stepList))]

polish (ApplyUnknown [] (y:ys)) =

       let [(ApplyDelta stepList)] = polish (ApplyUnknown [] ys)
        in [(ApplyDelta (ApplySL (ApplyIns y) stepList))]

polish (ApplyUnknown [x] [(y,step)]) =

       [(ApplyDelta (ApplySL (ApplyChg (ApplySingle x (y,step))) ApplyEmptyStepList))
       ,(ApplyDelta (ApplySL (ApplyDel x) (ApplySL (ApplyIns (y,step)) ApplyEmptyStepList)))
       ,(ApplyDelta (ApplySL (ApplyIns (y,step)) (ApplySL (ApplyDel x) ApplyEmptyStepList)))
       ]

polish (ApplyUnknown xs ys) =

       let allpairs = (tlist_tslist xs ys)

        in ins_Applycands [] (allways allpairs xs ys)   
---------------------------------------------------------- 
--polish (ApplySingle x y) =
  --     [(ApplySingle x y)]

polish (ApplyDelta stepList) = wrap_ApplyDelta (polish_ApplyStepList stepList)
----------------------------------------------------------
polish_ApplyStepList :: ApplyStepList -> [ApplyStepList]
polish_ApplyStepList (ApplyUnknownRest xs ys) = release_ApplyStepList (polish (ApplyUnknown xs ys)) 
polish_ApplyStepList ApplyEmptyStepList = []
polish_ApplyStepList (ApplySL x xs) = 
                     if fst (bounds_ApplyStep x) == snd (bounds_ApplyStep x)
                     then extend_ApplyStepList x (polish_ApplyStepList xs)
                     else combine_ApplyStepList (polish_ApplyStep x) xs 
----------------------------------------------------------
polish_ApplyStep :: ApplyStep -> [ApplyDelta]
polish_ApplyStep (ApplyChg d) = polish d
polish_ApplyStep _ = error "ApplyIns or ApplyDel does not need to be polished"
-----------------------------------------------------------------------------------------------
applySolutions :: [ApplyDelta] -> [ApplyDelta]

applySolutions [] = []

applySolutions (current_Applydelta:cands) =

     if fst (bounds_ApplyDelta current_Applydelta) /= snd (bounds_ApplyDelta current_Applydelta)
     then let extends = polish current_Applydelta
              inserts = ins_Applycands cands extends
           in applySolutions inserts
     else current_Applydelta:(applySolutions cands)

