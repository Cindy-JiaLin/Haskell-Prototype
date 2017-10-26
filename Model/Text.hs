module Model.Text
( lParagraph
, lT
, lText
)
where 

import Data.List

import Main.Value

type Weight = Double
-- create a list of (S n String) from a paragraph
--Using the words function
-- *Model.Text> :t words
-- words :: String -> [String]
lParagraph :: Weight -> Int -> String -> T
lParagraph w n par = lS w n (words par)

lT :: Weight -> Int -> [String] -> TypedList
lT _ _ [] = EmptyList
lT w n (l:ls) = Lst (lParagraph w n l) (lT w n ls)

lText :: Weight -> Int -> [String] -> T
lText _ _ [] = List EmptyList
lText w n text = List (lT w n text) 
