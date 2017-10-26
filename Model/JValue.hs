module Model.JValue
(JValue (..)
,getObj
,sameJValue
)
where 

data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber Double
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Ord, Show)

getObj :: JValue -> [(String, JValue)]
getObj (JObject xs) = xs

-- This sameJValue function is used to test the type of two JValues   
sameJValue :: JValue -> JValue -> Bool
sameJValue JNull JNull = True
sameJValue (JBool _) (JBool _) = True
sameJValue (JString _) (JString _) = True
sameJValue (JNumber _) (JNumber _) = True
sameJValue (JArray _) (JArray _) = True
sameJValue (JObject _) (JObject _) = True
sameJValue _ _ = False

