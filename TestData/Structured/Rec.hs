module TestData.Structured.Rec where

import Data.List

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

import Main.Application.Apply

----------------------------------------------------------------------------------------------
-- Implementation on page 120
leaf3 = Rec (Pair (I 1 1 3) (List EmptyList))
leaf4 = Rec (Pair (I 1 1 4) (List EmptyList))
leaf5 = Rec (Pair (I 1 1 5) (List EmptyList))
leaf6 = Rec (Pair (I 1 1 6) (List EmptyList))
leaves_of_2 = listT [leaf4, leaf5, leaf6]
tree2 = Rec (Pair (I 1 1 2) leaves_of_2)
branches_of_1 = listT [tree2, leaf3]
tree1 = Rec (Pair (I 1 1 1) branches_of_1)


