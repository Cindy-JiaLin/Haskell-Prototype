module TestData.HumanRights10 where

import Data.List

import Model.Text

import Main.Value
import Main.Delta
import Main.Solution
import Main.Omega

unorg_10 = "Article 1.\nAll human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood.\nArticle 2.\nEveryone is entitled to all the rights and freedoms set forth in this Declaration, without distinction of any kind, such as race, colour, sex, language, religion, political or other opinion, national or social origin, property, birth or other status. Furthermore, no distinction shall be made on the basis of the political, jurisdictional or international status of the country or territory to which a person belongs, whether it be independent, trust, non-self-governing or under any other limitation of sovereignty.\nArticle 3.\nEveryone has the right to life, liberty and security of person.\nArticle 4.\nNo one shall be held in slavery or servitude; slavery and the slave trade shall be prohibited in all their forms.\nArticle 5.\nNo one shall be subjected to torture or to cruel, inhuman or degrading treatment or punishment.\nArticle 6.\nEveryone has the right to recognition everywhere as a person before the law.\nArticle 7.\nAll are equal before the law and are entitled without any discrimination to equal protection of the law. All are entitled to equal protection against any discrimination in violation of this Declaration and against any incitement to such discrimination.\nArticle 8.\nEveryone has the right to an effective remedy by the competent national tribunals for acts violating the fundamental rights granted him by the constitution or by law.\nArticle 9.\nNo one shall be subjected to arbitrary arrest, detention or exile.\nArticle 10.\nEveryone is entitled in full equality to a fair and public hearing by an independent and impartial tribunal, in the determination of his rights and obligations and of any criminal charge against him."

buzzlecom_10 = "Article 1 -- Right To Equality\nAll human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood.\nArticle 2 -- Freedom From Discrimination\nEveryone is entitled to all the rights and freedoms set forth in this Declaration, without distinction of any kind, such as race, color, sex, language, religion, political or other opinion, national or social origin, property, birth or other status. Furthermore, no distinction shall be made on the basis of the political, jurisdictional or international status of the country or territory to which a person belongs, whether it be independent, trust, non-self-governing or under any other limitation of sovereignty.\nArticle 3 -- Right to Security of Person\nEveryone has the right to life, liberty and security of person.\nArticle 4 -- Freedom from Slavery\nNo one shall be held in slavery or servitude; slavery and the slave trade shall be prohibited in all their forms.\nArticle 5 -- Freedom From Inhumane Treatment\nNo one shall be subjected to torture or to cruel, inhumane or degrading treatment or punishment.\nArticle 6 -- Right To Legal Recognition\nEveryone has the right to be recognized as a person before the law.\nArticle 7 -- Right To Equality Before the Law\nAll are equal before the law and are entitled without any discrimination to equal protection of the law. All are entitled to equal protection against any discrimination in violation of this Declaration and against any incitement to such discrimination.\nArticle 8 -- Right To Remedy by Competent Tribunal\nEveryone has the right to an effective remedy by the competent national tribunals for acts violating the fundamental rights granted to him by the constitution or by law.\nArticle 9 -- Freedom From Arbitrary Legal Prosecution\nNo one shall be subjected to arbitrary arrest, detention or exile.\nArticle 10 -- Right To Fair Public Hearing\nEveryone is entitled in full equality to a fair and public hearing by an independent and impartial tribunal, in the determination of his rights and obligations and of any criminal charge against him."

list1 = lText 1 1 (lines unorg_10)
list2 = lText 1 1 (lines buzzlecom_10)

delta_10 = ω list1 list2 1
delta_10_half = ω list1 list2 0.5


{-
-- Result of String accuracy == 3
*TestData.HumanRights10> delta_10

(Delta_List: 
[ Delta_List: ["=", ≈("1.", "1"), +"--", +"Right", +"To", +"Equality"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("2.", "2"), +"--", +"Freedom", +"From", +"Discrimination"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", ≈("colour,", "color,"), "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("3.", "3"), +"--", +"Right", +"to", +"Security", +"of", +"Person"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("4.", "4"), +"--", +"Freedom", +"from", +"Slavery"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("5.", "5"), +"--", +"Freedom", +"From", +"Inhumane", +"Treatment"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", ≈("inhuman", "inhumane"), "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("6.", "6"), +"--", +"Right", +"To", +"Legal", +"Recognition"]
, Delta_List: ["=", "=", "=", "=", "=", -"recognition", -"everywhere", ≈("as", "be"), +"recognized", ≈("a", "as"), +"a", "=", "=", "=", "="]
, Delta_List: ["=", ≈("7.", "7"), +"--", +"Right", +"To", +"Equality", +"Before", +"the", +"Law"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("8.", "8"), +"--", +"Right", +"To", +"Remedy", +"by", +"Competent", +"Tribunal"]

, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", ≈("him", "to"), ≈("by", "him"), ≈("the", "by"), -"constitution", ≈("or", "the"), +"constitution", ≈("by", "or"), +"by", "="]

, Delta_List: ["=", ≈("9.", "9"), +"--", +"Freedom", +"From", +"Arbitrary", +"Legal", +"Prosecution"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("10.", "10"), +"--", +"Right", +"To", +"Fair", +"Public", +"Hearing"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
]
,0.8844386853575215)
(290.66 secs, 95051847744 bytes)

-}

{-
-- Result of String accuracy == 1
*TestData.HumanRights10> delta_10

(Delta_List: 
[Delta_List: ["=", ≈("1.", "1"), +"--", +"Right", +"To", +"Equality"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("2.", "2"), +"--", +"Freedom", +"From", +"Discrimination"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", (-"colour,", +"color,"), "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("3.", "3"), +"--", +"Right", +"to", +"Security", +"of", +"Person"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("4.", "4"), +"--", +"Freedom", +"from", +"Slavery"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("5.", "5"), +"--", +"Freedom", +"From", +"Inhumane", +"Treatment"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", ≈("inhuman", "inhumane"), "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("6.", "6"), +"--", +"Right", +"To", +"Legal", +"Recognition"]
, Delta_List: ["=", "=", "=", "=", "=", (-"recognition", +"be"), (-"everywhere", +"recognized"), "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("7.", "7"), +"--", +"Right", +"To", +"Equality", +"Before", +"the", +"Law"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("8.", "8"), +"--", +"Right", +"To", +"Remedy", +"by", +"Competent", +"Tribunal"]

, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", +"to", "=", "=", "=", "=", "=", "=", "="]

, Delta_List: ["=", ≈("9.", "9"), +"--", +"Freedom", +"From", +"Arbitrary", +"Legal", +"Prosecution"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
, Delta_List: ["=", ≈("10.", "10"), +"--", +"Right", +"To", +"Fair", +"Public", +"Hearing"]
, Delta_List: ["=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "=", "="]
],0.8965798876978051)
(637.02 secs, 210622163472 bytes)


-}


