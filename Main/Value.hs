module Main.Value 
( T (..)
--------
, getBool
, getStr
, getReal
, first
, second
--------
, getRec
, getJSON
, eq
, weight_Value
-----------------
, UnionType (..)
-----------------
, getTypedSet
, getTypedMSet
, getTypedList
, getTypedMap
-----------------
, TypedSet (..)
, isEmptySet
, add
, getRest
, union_TypedSet
-----------------
, TypedMSet (..)
, isEmptyMSet
, put
, getMRest
, domSet
, eq_TypedMSet
, sub_TypedMSet
, less_TypedMSet
, inter_TypedMSet
, union_TypedMSet
, mix_TypedMSet
-----------------
, TypedList (..)
, ins
, indexed_to_TypedSet
------------------
, TypedMap (..)
, eq_TypedMap
, extend
, map_to_set
, set_to_map
------------------
, JValue (..)
, isJSON
, isSameJSON
, ljv
, to_ljv
, ljo
, ljo_to_set
, set_to_ljo
-------------------
, lI
, lR
, lS
, lC
, sI
, sR
, sS
, mI
, mR
, mS
, set
, list
--, indexedList
, setT
, msetT
, listT
, mapT
) where

import Data.List
import Main.Eq
import Model.JValue

type Eps = Double
type N = Int
type Weight = Double

data T = Unit
       | B Weight Bool
       | C Weight Char
       | Nat Weight Int 
       | R Weight Eps Double
       | I Weight N Int
       | S Weight N String
       | Pair T T
       | Union UnionType
       | Set TypedSet
       | MSet TypedMSet
       | List TypedList
       | Mapping TypedMap
       | Rec T
       | JSON JValue 
         deriving (Eq, Ord)

instance Show T where
    show Unit = "unit"
    show (B w x) = show x
    show (C w x) = show x
    show (Nat w x) = (show x)
    show (I w n x) = (show x) -- ++ "("++(show n)++")"
    show (R w ε x)  = (show x) -- ++ "("++(show ε)++")"
    show (S w n x) = (show x) -- ++ "("++(show n)++")"
    show (Pair x y) = "("++(show x)++","++(show y)++")"
    show (Union x) = show x
    show (Set x) = show x
    show (MSet x) = show x
    show (List x) = show x
    show (Mapping x) = show x
    show (Rec x) = show x
    show (JSON x) ="JSON: "++(show x)

--------------------------------------
getBool :: T -> Bool
getBool (B _ x) = x

getStr :: T -> String
getStr (S _ _ x) = x

getReal :: T -> Double
getReal (R _ _ x) = x
--------------------------------------
first :: T -> T
first (Pair x y) = x

second :: T -> T
second (Pair x y) = y

getRec :: T -> T
getRec (Rec x) = x

getJSON :: T -> JValue
getJSON (JSON x) = x
--------------------------------------
eq_JValue :: JValue -> JValue -> Bool 
eq_JValue JNull JNull = True
eq_JValue (JBool x) (JBool y) = x == y
eq_JValue (JString x) (JString y) = x == y
eq_JValue (JNumber x) (JNumber y) = eqDouble x y
eq_JValue (JArray []) (JArray []) = True
eq_JValue (JArray []) (JArray ys) = False
eq_JValue (JArray xs) (JArray []) = False
eq_JValue (JArray (x:xs)) (JArray (y:ys)) = 
          if (eq_JValue x y) then eq_JValue (JArray xs) (JArray ys)
          else False
eq_JValue (JObject []) (JObject []) = True
eq_JValue (JObject []) (JObject ys) = False
eq_JValue (JObject xs) (JObject []) = False
eq_JValue (JObject xs) (JObject ys) = 
          let set_xs = jObject_to_TypedSet xs
              set_ys = jObject_to_TypedSet ys
           in eq_TypedSet set_xs set_ys 
        
jObject_to_TypedSet :: [(String, JValue)] -> TypedSet
jObject_to_TypedSet [] = EmptySet
jObject_to_TypedSet ((x,jvx):xs) = Add (Pair (JSON (JString x)) (JSON jvx)) (jObject_to_TypedSet xs)

isJSON :: T -> Bool
isJSON (JSON _) = True
isJSON _ = False

isSameJSON :: T -> T -> Bool
isSameJSON (JSON x) (JSON y) = sameJValue x y
isSameJSON _ _ = error "They are not JSON type, Value.isSameJSON." 
-- This ljv (list of JValue) function is used to convert a list of JValues to a TypedList
ljv :: [JValue] -> TypedList
ljv [] = EmptyList
ljv (x:xs) = Lst (JSON x) (ljv xs)

to_ljv :: TypedList -> [JValue]
to_ljv EmptyList = []
to_ljv (Lst (JSON x) xs) = x:(to_ljv xs)

-- This ljo (list of pair of String and JValue) function is used to convert such structure to a TypedMap
ljo :: [(String, JValue)] -> TypedMap
ljo [] = EmptyMap
ljo ((s,jv):rest) = Env (S 1 3 s) (JSON jv) (ljo rest)

-- This ljo_to_set function is used to convert a JObject to a set of pairs
-- A JObject also can be modelled as a mapping, but here we don't use it.
-- Because of the properties of Mappings depend on the corresponding properties of set of pairs

ljo_to_set :: [(String, JValue)] -> TypedSet
ljo_to_set [] = EmptySet
ljo_to_set ((s,jv):rest) = Add (Pair (S 1 3 s) (JSON jv)) (ljo_to_set rest)

set_to_ljo :: TypedSet -> [(String, JValue)]
set_to_ljo EmptySet = []
set_to_ljo (Add (Pair x (JSON y)) rest) = ((getStr x), y):(set_to_ljo rest)
set_to_ljo (Add (Pair x y) rest) = ((getStr x), (JString (getStr y))):(set_to_ljo rest)


weight_JSON :: JValue -> Double
weight_JSON JNull = 0
weight_JSON (JBool x) = 1
weight_JSON (JString x) = 1
weight_JSON (JNumber x) = 1
weight_JSON (JArray xs) = weight_TypedList (ljv xs)
weight_JSON (JObject xs) = weight_TypedMap (ljo xs)

--------------------------------------
data UnionType = Lt T | Rt T
             deriving (Eq, Ord)

instance Show UnionType where
    show (Lt x) = "Left " ++ (show x)
    show (Rt x) = "Right " ++ (show x)

weight_Union :: UnionType -> Double
weight_Union (Lt x) = weight_Value x
weight_Union (Rt x) = weight_Value x

-- This sameUnion function is used to judge that if two union types are the same
-- This function is on the type perspective
-- hence the only one side is the same type cannot determine the other side
-- when the give value of the union type are come from different side
-- these two value cannot be compared
sameUnion :: UnionType -> UnionType -> Bool
sameUnion (Lt x) (Lt y) = sameType x y 
sameUnion (Rt x) (Rt y) = sameType x y
sameUnion (Lt _) (Rt _) = error "LR:Different side of union cannot be compared"
sameUnion (Rt _) (Lt _) = error "RL:Different side of union cannot be compared"

-- This eqUnion function is used to compare the value of union types
-- These two values must come from the same side
eqUnion :: UnionType -> UnionType -> Bool
eqUnion (Lt x) (Lt y) = eq x y
eqUnion (Rt x) (Rt y) = eq x y
eqUnion (Lt _) (Rt _) = error "LR:Different side of union cannot be compared"
eqUnion (Rt _) (Lt _) = error "RL:Different side of union cannot be compared"
------------------------------------------------------------------------------
--data RecType = EmptyRec 
  --           | Unfold T
    --           deriving (Eq, Ord, Show)
------------------------------------------------------------------------------------------------
-- This weight_Value function is used to identify the actual weight of objects of different types.

weight_Value :: T -> Double 
weight_Value Unit = 1
weight_Value (B w t) = w
weight_Value (C w x) = w
weight_Value (Nat w x) = w
weight_Value (I w n x) = w 
weight_Value (R w ε x) = w
weight_Value (S w n x) = w
weight_Value (Pair x y) = (weight_Value x) + (weight_Value y)
weight_Value (Union x) = weight_Union x
weight_Value (Set x) = weight_TypedSet x 
weight_Value (MSet x) = weight_TypedMSet x
weight_Value (List x) = weight_TypedList x
weight_Value (Mapping x) = weight_TypedMap x
weight_Value (Rec x) = weight_Value x
weight_Value (JSON x) = weight_JSON x

----------------------------------------------------------------------
getTypedSet :: T -> TypedSet
getTypedSet (Set xs) = xs

getTypedMSet :: T -> TypedMSet
getTypedMSet (MSet xs) = xs

getTypedList :: T -> TypedList
getTypedList (List xs) = xs

getTypedMap :: T -> TypedMap
getTypedMap (Mapping xs) = xs 
----------------------------------------------------------------------

-- This sameType function is used to judge if two objects are of the same type.
sameType :: T -> T -> Bool
sameType Unit Unit = True
sameType (B _ _) (B _ _) = True
sameType (C _ _) (C _ _) = True
sameType (Nat _ _) (Nat _ _) = True
sameType (I _ n1 _) (I _ n2 _) = if n1 == n2 then True else False
sameType (R _ ε1 _) (R _ ε2 _) = if eqDouble ε1 ε2 then True else False
sameType (S _ n1 _) (S _ n2 _) = if n1 == n2 then True else False
sameType (Pair x y) (Pair u v) = (sameType x u) && (sameType y v)
sameType (Union x) (Union y) = sameUnion x y
sameType (Set x) (Set y) = sameTypedSet x y
sameType (MSet x) (MSet y) = sameTypedMSet x y
sameType (List x) (List y) = sameTypedList x y
sameType (Mapping x) (Mapping y) = sameTypedMap x y
sameType (Rec x) (Rec y) = True 
sameType (JSON x) (JSON y) = True 
sameType _ _ = False

-- This eq function is used to determine if two objects of the same type are identical.
eq :: T -> T -> Bool
eq Unit Unit = True
eq (B w1 t1) (B w2 t2) = (t1 == t2) && (eqDouble w1 w2)
eq (C w1 x1) (C w2 x2) = (x1 == x2) && (eqDouble w1 w2)
eq (Nat w1 x1) (Nat w2 x2) = (x1 == x2) && (eqDouble w1 w2)
eq (I w1 n1 x) (I w2 n2 y) = (n1 == n2) && (x == y) && (eqDouble w1 w2)
eq (R w1 ε1 x) (R w2 ε2 y) = (eqDouble ε1 ε2) && (eqDouble x y) && (eqDouble w1 w2)
eq (S w1 n1 x) (S w2 n2 y) = (n1 == n2) && (x == y) && (eqDouble w1 w2)
eq (Pair x y) (Pair u v) = (eq x u) && (eq y v)
eq (Union x) (Union y) = eqUnion x y
eq (Set x) (Set y) = eq_TypedSet x y
eq (MSet x) (MSet y) = eq_TypedMSet x y
eq (List x) (List y) = eq_TypedList x y
eq (Mapping x) (Mapping y) = eq_TypedMap x y
eq (Rec x) (Rec y) = eq x y
eq (JSON x) (JSON y) = eq_JValue x y
eq _ _ = False
----------------------------------------------------------------------
-- TypedSet
data TypedSet = EmptySet
              | Add T TypedSet
              deriving (Eq, Ord)

instance Show TypedSet where
    show set ="{" ++ (showSet set) ++ "}" where
                      showSet EmptySet = ""
                      showSet (Add x EmptySet) = show x
                      showSet (Add x xs) = (show x)++", "++(showSet xs)

isEmptySet :: TypedSet -> Bool
isEmptySet EmptySet = True
isEmptySet _ = False

add :: T -> TypedSet -> TypedSet
add a EmptySet = Add a EmptySet
add a (Add x xs) = if (not (sameType a x))
                   then error "Invalid data type cannot be added to typedset"
                   else if eq a x then Add x xs else Add x (add a xs)

-- operations of TypedSet
hasElement :: TypedSet -> T -> Bool
hasElement EmptySet _ = False
hasElement (Add x xs) a = if (not (sameType x a))
                          then error "Invalid data type in Value.hasElement function"
                          else if (eq x a) then True
                               else hasElement xs a

getRest :: TypedSet -> T -> TypedSet
getRest EmptySet _ = EmptySet
getRest (Add x xs) a = if (not (sameType x a)) || (not (hasElement (Add x xs) a))
                       then error "This object is not an element in this TypedSet."
                       else ( if (eq x a) then xs
                              else Add x (getRest xs a)
                            )

weight_TypedSet :: TypedSet -> Double 
weight_TypedSet EmptySet = 0 
weight_TypedSet (Add x xs) = (weight_Value x) + (weight_TypedSet xs)
-----------------------------------------------------
-- This subset function is used to 
-- test if the second set is a subset of the first set.
subset :: TypedSet -> TypedSet -> Bool
subset _ EmptySet = True
subset EmptySet _ = False
subset (Add x xs) (Add y ys) =
          if (not (hasElement (Add x xs) y)) 
          then False
          else subset (Add x xs) ys

-- This sameTypedSet function is used to test if two typedsets are of the same type. 
sameTypedSet :: TypedSet -> TypedSet -> Bool
sameTypedSet EmptySet _ = True
sameTypedSet _ EmptySet = True
sameTypedSet (Add x xs) (Add y ys) = sameType x y

-- This eq_TypedSet function is used to test if two typedsets are equal.
eq_TypedSet :: TypedSet -> TypedSet -> Bool
eq_TypedSet EmptySet EmptySet = True
eq_TypedSet EmptySet _ = False
eq_TypedSet _ EmptySet = False
eq_TypedSet set1 set2 = if (not (sameTypedSet set1 set2)) then False
                        else (subset set1 set2) && (subset set2 set1) 

less_TypedSet :: TypedSet -> TypedSet -> TypedSet
less_TypedSet EmptySet _ = EmptySet
less_TypedSet set EmptySet = set
less_TypedSet (Add x xs) set =
              if hasElement set x
              then less_TypedSet xs set
              else Add x (less_TypedSet xs set) 

inter_TypedSet :: TypedSet -> TypedSet -> TypedSet
inter_TypedSet set1 set2 = less_TypedSet set1 (less_TypedSet set1 set2) 

union_TypedSet :: TypedSet -> TypedSet -> TypedSet
union_TypedSet xs EmptySet = xs
union_TypedSet EmptySet xs = xs
union_TypedSet xs (Add y ys) = union_TypedSet (add y xs) ys
----------------------------------------------------------------------
-- TypedMSet
data TypedMSet = EmptyMSet
               | Put T TypedMSet
               deriving (Eq, Ord)

instance Show TypedMSet where
    show mset ="<" ++ (showMSet mset) ++ ">" where
                      showMSet EmptyMSet = ""
                      showMSet (Put x EmptyMSet) = show x
                      showMSet (Put x xs) = show x ++ ", "++(showMSet xs)

isEmptyMSet :: TypedMSet -> Bool
isEmptyMSet EmptyMSet = True
isEmptyMSet _ = False

put :: T -> TypedMSet -> TypedMSet
put a EmptyMSet = Put a EmptyMSet
put a (Put x xs) = 
    if (not (sameType a x))
    then error "Invalid data type cannot be put into typedmset"
    else if eq a x then Put a (Put x xs) else Put x (put a xs)

multi_put :: T -> Int -> TypedMSet -> TypedMSet 
multi_put _ 0 mset = mset
multi_put x n mset = put x (multi_put x (n-1) mset)


freq :: TypedMSet -> T -> Int
freq EmptyMSet _ = 0
freq (Put x xs) a = 
     if (not (sameType a x))
     then error "Invalid data type in Value.freq function"
     else if (eq x a) then 1+(freq xs a) 
          else freq xs a
                             
hasMElement :: TypedMSet -> T -> Bool
hasMElement EmptyMSet _ = False
hasMElement (Put x xs) a = 
            if (not (sameType x a))
            then error "Invalid data type in Value.hasMElement function"
            else if (eq x a) then True
                 else hasMElement xs a

getMRest :: TypedMSet -> T -> TypedMSet
getMRest EmptyMSet _ = EmptyMSet
getMRest (Put x xs) a = if (not (sameType x a)) || (not (hasMElement (Put x xs) a))
                       then error "This object is not an element in this TypedMSet."
                       else ( if (eq x a) then xs
                              else Put x (getMRest xs a)
                            )


weight_TypedMSet :: TypedMSet -> Double 
weight_TypedMSet EmptyMSet = 0 
weight_TypedMSet (Put x xs) = (weight_Value x) + (weight_TypedMSet xs)

-- This domSet function is used to get the base set of a multiset
-- in which the result set contains all elements (non-duplicated) in this multiset
domSet :: TypedMSet -> TypedSet
domSet EmptyMSet = EmptySet
domSet (Put x xs) = add x (domSet xs)
-----------------------------------------------------
-- This sameTypedMSet function is used to test if two typedmsets are of the same type. 
sameTypedMSet :: TypedMSet -> TypedMSet -> Bool
sameTypedMSet EmptyMSet _ = True
sameTypedMSet _ EmptyMSet = True
sameTypedMSet (Put x xs) (Put y ys) = sameType x y
 
-- This eq_TypedMSet function is used to test if two typedmsets are equal.
eq_TypedMSet :: TypedMSet -> TypedMSet -> Bool
eq_TypedMSet mset1 mset2 = 
             let dom_mset1 = domSet mset1
                 dom_mset2 = domSet mset2
              in if (not (eq_TypedSet dom_mset1 dom_mset2))
                 then False
                 else isEq mset1 mset2 dom_mset1 where
                      isEq :: TypedMSet -> TypedMSet -> TypedSet -> Bool
                      isEq _ _ EmptySet = True
                      isEq EmptyMSet _ _ = False
                      isEq _ EmptyMSet _ = False
                      isEq mset1 mset2 (Add x xs) = 
                           if (freq mset1 x) == (freq mset2 x)
                           then isEq mset1 mset2 xs
                           else False
            
-- This less_TypedMSet function is used to return the difference A\B
less_TypedMSet :: TypedMSet -> TypedMSet -> TypedMSet
less_TypedMSet mset1 mset2 = 
               let dom_mset2 = domSet mset2
                in lessMSet mset1 mset2 dom_mset2 where
                   lessMSet :: TypedMSet -> TypedMSet -> TypedSet -> TypedMSet
                   lessMSet EmptyMSet _ _ = EmptyMSet
                   lessMSet mset EmptyMSet _ = mset
                   lessMSet (Put x xs) mset dom =
                            if hasElement dom x
                            then let m = (freq xs x)+1
                                     n = freq mset x
                                  in if (m-n > 0)
                                     then multi_put x (m-n) (lessMSet xs mset dom) 
                                     else lessMSet xs mset dom
                            else Put x (lessMSet xs mset dom)
                                      

-- This sub_TypedMSet function is used to point out that 
-- the first mset is the sub multiset of the second one
sub_TypedMSet :: TypedMSet -> TypedMSet -> Bool
sub_TypedMSet EmptyMSet _ = True
sub_TypedMSet _ EmptyMSet = False
sub_TypedMSet mset1 mset2 =
              let dom_mset1 = domSet mset1
               in isSub mset1 mset2 dom_mset1 where
                  isSub :: TypedMSet -> TypedMSet -> TypedSet -> Bool
                  isSub _ _ EmptySet = True
                  isSub mset1 mset2 (Add x xs) =
                        let m = freq mset1 x
                            n = freq mset2 x
                         in if m > n then False
                            else isSub mset1 mset2 xs

-- This inter_TypedMSet function is used to get the 
-- intersectio of multisets
inter_TypedMSet :: TypedMSet -> TypedMSet -> TypedMSet
inter_TypedMSet mset1 mset2 = 
                less_TypedMSet mset1 (less_TypedMSet mset1 mset2)

-- This union_TypedMSet function is used to get the union of two multisets
union_TypedMSet :: TypedMSet -> TypedMSet -> TypedMSet
union_TypedMSet EmptyMSet mset = mset
union_TypedMSet mset EmptyMSet = mset
union_TypedMSet mset1 mset2 =
                if isEmptyMSet (less_TypedMSet mset1 mset2)
                then mset2
                else let (Put x xs) = less_TypedMSet mset1 mset2
                      in put x (union_TypedMSet xs mset2)
-- This mix_TypedMSet function is used to put elements in the second multiset
-- to the first multiset to build a new multiset.
-- It will be used in the Apply.applyForward for multisets
-- It is the essential step in the Apply.applyForward for multisets
mix_TypedMSet :: TypedMSet -> TypedMSet -> TypedMSet
mix_TypedMSet mset EmptyMSet = mset
mix_TypedMSet EmptyMSet mset = mset
mix_TypedMSet mset (Put x xs) = put x (mix_TypedMSet mset xs)
-----------------------------------------------------------------------  
-----------------------------------------------------------------------          
data TypedList = EmptyList
               | Lst T TypedList
               deriving (Eq, Ord)
instance Show TypedList where
    show l = "[" ++ (showLst l) ++ "]" where
                     showLst EmptyList = ""
                     showLst (Lst x EmptyList) = show x
                     showLst (Lst x xs) = (show x)++", "++(showLst xs)

ins :: T -> TypedList -> TypedList
ins a EmptyList = Lst a EmptyList
ins a (Lst x xs) = if (not (sameType a x))
                   then error "Invalid element type inserted to the typedlist"
                   else Lst a (Lst x xs)

weight_TypedList :: TypedList -> Double 
weight_TypedList EmptyList = 0 
weight_TypedList (Lst x xs) = (weight_Value x) + (weight_TypedList xs) 


sameTypedList :: TypedList -> TypedList -> Bool
sameTypedList EmptyList _ = True
sameTypedList _ EmptyList = True
sameTypedList (Lst x xs) (Lst y ys) = if (not (sameType x y))
                                      then False
                                      else sameTypedList xs ys

eq_TypedList :: TypedList -> TypedList -> Bool
eq_TypedList EmptyList EmptyList = True
eq_TypedList EmptyList _ = False
eq_TypedList _ EmptyList = False
eq_TypedList (Lst x xs) (Lst y ys) = 
             if (eq x y) then eq_TypedList xs ys
             else False

size :: TypedList -> Int
size EmptyList = 0
size (Lst x xs) = 1 + (size xs)

-- This indexed_to_TypedSet function is used to convert a list of element T
-- to a set of indexed element whose type is (Pair I T)
indexed_to_TypedSet :: TypedList -> TypedSet
indexed_to_TypedSet EmptyList = EmptySet
indexed_to_TypedSet (Lst x xs) = Add (Pair (I 0 1 ((size xs)+1)) x) (indexed_to_TypedSet xs)
-----------------------------------------------------------------------------------------------
data TypedMap = EmptyMap
              | Env T T TypedMap
              deriving (Eq, Ord)

instance Show TypedMap where
   show m = "{" ++ (showMap m) ++ "}" where
                    showMap EmptyMap = ""
                    showMap (Env x y EmptyMap) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"
                    showMap (Env x y tmap) = "(" ++ (show x) ++ ", " ++ (show y) ++ "), " ++ (showMap tmap)

extend :: T -> TypedMap -> TypedMap
extend (Pair a b) EmptyMap = Env a b EmptyMap
extend (Pair a b) (Env x y tmap) = if (eq a x) then Env x y tmap
                                   else Env x y (extend (Pair a b) tmap) 
 
hasPair :: TypedMap -> T -> T -> Bool
hasPair EmptyMap _ _ = False
hasPair (Env x y tmap) a b = 
           if (eq x a) && (eq y b) then True
           else hasPair tmap a b

weight_TypedMap :: TypedMap -> Double 
weight_TypedMap EmptyMap = 0 
weight_TypedMap (Env x y tmap) = (weight_Value x) + (weight_Value y) + (weight_TypedMap tmap)

sameTypedMap :: TypedMap -> TypedMap -> Bool
sameTypedMap map1 map2 = 
             sameTypedSet (map_to_set map1) (map_to_set map2) 
         
eq_TypedMap :: TypedMap -> TypedMap -> Bool
eq_TypedMap map1 map2 = 
            eq_TypedSet (map_to_set map1) (map_to_set map2) 

-- This function is used to convert a mapping to a set of pairs
map_to_set :: TypedMap -> TypedSet
map_to_set EmptyMap = EmptySet
map_to_set (Env x y tmap) = Add (Pair x y) (map_to_set tmap)

-- This function is used to convert a set of pairs to a mapping
-- It is particularly used in apply facility
set_to_map :: TypedSet -> TypedMap
set_to_map EmptySet = EmptyMap
set_to_map (Add (Pair x y) rest) = Env x y (set_to_map rest) 
---------------------------------------------------------            
-- ??? geometric x = 0.99*x
----------------------------------------------------------------------
-- lI create a list of integer
-- l is a accumulator
lI :: Weight -> N -> [Int] -> T
lI w n lst = List (foldr (\x -> \l -> (ins (I w n x) l)) EmptyList lst)

-- lR create a list of Double (Real Number)
lR :: Weight -> Eps -> [Double] -> T
lR w ε lst = List (foldr (\x -> \l -> (ins (R w ε x) l)) EmptyList lst)
  
-- lS create a list of string
lS :: Weight -> N -> [String] -> T
lS w n lst = List (foldr (\x -> \l -> (ins (S w n x) l)) EmptyList lst)

-- lC create a list of characters
lC :: Weight -> [Char] -> T
lC w lst = List (foldr (\x -> \l -> (ins (C w x) l)) EmptyList lst)


-- sI create a set of integer
sI :: Weight -> N -> [Int] -> T
sI w n lst = Set (foldr (\x -> \s -> (add (I w n x) s)) EmptySet lst)
-- sR create a set of real numbers (Double)
sR :: Weight -> Eps -> [Double] -> T
sR w ε lst = Set (foldr (\x -> \s -> (add (R w ε x) s)) EmptySet lst)
-- sS create a set of string
sS :: Weight -> N -> [String] -> T
sS w n lst = Set (foldr (\x -> \s -> (add (S w n x) s)) EmptySet lst)

-- mI create a multiset of integer
mI :: Weight -> N -> [Int] -> T
mI w n lst = MSet (foldr (\x -> \s -> (put (I w n x) s)) EmptyMSet lst)
-- mR create a multiset of real numbers (Double)
mR :: Weight -> Eps -> [Double] -> T
mR w ε lst = MSet (foldr (\x -> \s -> (put (R w ε x) s)) EmptyMSet lst)
-- mS create a multiset of string
mS :: Weight -> N -> [String] -> T
mS w n lst = MSet (foldr (\x -> \s -> (put (S w n x) s)) EmptyMSet lst)

-- set create a set from a string
-- by import Data.List
-- *Model.Text> :t group
-- group :: ByteString -> [ByteString] 
set :: Weight -> N -> String -> T
set w n l = Set (foldr (\x -> \s -> (add (S w n x) s)) EmptySet (words l))

list :: Weight -> N -> String -> T
list w n str = List (foldr (\x -> \l -> (ins (S w n x) l)) EmptyList (words str))

indexedList :: Weight -> N -> String -> T
indexedList w n str = 
 
  List (foldr (\x -> \l -> (ins (Pair (I 0 1 ((size l)+1)) (S w n x)) l)) EmptyList (words str))


setT :: [T] -> T
setT l = Set (foldr (\x -> \s ->(add x s)) EmptySet l)

msetT :: [T] -> T
msetT l = MSet (foldr (\x -> \s -> (put x s)) EmptyMSet l)

listT :: [T] -> T
listT l = List (foldr (\x -> \s -> (ins x s)) EmptyList l)

{-
indexedListT :: [T] -> T
indexedListT l = 
     List (foldr (\x -> \s -> (ins (Pair (I 0 1 ((size s)+1)) x) s)) EmptyList l)
-}

mapT :: (T -> T) -> [T] -> T
mapT f l = Mapping (foldr (\x -> \m -> (extend (Pair x (f x)) m)) EmptyMap l)
---------------------------------------------------------------------------------


