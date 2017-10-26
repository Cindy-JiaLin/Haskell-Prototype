
-- This is the code generation of examples in TestData.Structured.List
unknown = Unknown lp1 lp2
refine0 = refine unknown

extend0 = refine (head refine0)
insert0 = ins_cands (tail refine0) extend0

extend1 = refine (head insert0)
insert1 = ins_cands (tail insert0) extend1

extend2 = refine (head insert1)
insert2 = ins_cands (tail insert1) extend2

extend3 = refine (head insert2)
insert3 = ins_cands (tail insert2) extend3

extend4 = refine (head insert3)
insert4 = ins_cands (tail insert3) extend4

extend5 = refine (head insert4)
insert5 = ins_cands (tail insert4) extend5

extend6 = refine (head insert5)
insert6 = ins_cands (tail insert5) extend6

extend7 = refine (head insert6)
insert7 = ins_cands (tail insert6) extend7

extend8 = refine (head insert7)
insert8 = ins_cands (tail insert7) extend8

extend9 = refine (head insert8)
insert9 = ins_cands (tail insert8) extend9

extend10 = refine (head insert9)
insert10 = ins_cands (tail insert9) extend10

extend11 = refine (head insert10)
insert11 = ins_cands (tail insert10) extend11
{-

*TestData.Structured.List> insert11
[Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,<Delta_Pair: ("="2, "="2)>
             ,<Delta_Pair: ("="3, (-3, +2))>
             ,<Delta_Pair: ("="4, (-4, +5))>
             ,-(5,5)
             ,(unknown [], [(5,5)])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,<Delta_Pair: ("="2, "="2)>
             ,<Delta_Pair: ("="3, (-3, +2))>
             ,<Delta_Pair: ("="4, (-4, +5))>
             ,+(5,5)
             ,(unknown [(5,5)], [])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,<Delta_Pair: ("="2, "="2)>
             ,<Delta_Pair: ("="3, (-3, +2))>
             ,-(4,4)
             ,(unknown [(5,5)], [(4,5), (5,5)])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,<Delta_Pair: ("="2, "="2)>
             ,<Delta_Pair: ("="3, (-3, +2))>
             ,+(4,5)
             ,(unknown [(4,4), (5,5)], [(5,5)])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,<Delta_Pair: ("="2, "="2)>,-(3,3)
             ,(unknown [(4,4), (5,5)], [(3,2), (4,5), (5,5)])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,<Delta_Pair: ("="2, "="2)>
             ,+(3,2)
             ,(unknown [(3,3), (4,4), (5,5)], [(4,5), (5,5)])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,-(2,2)
             ,(unknown [(3,3), (4,4), (5,5)], [(2,2), (3,2), (4,5), (5,5)])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,+(2,2)
             ,(unknown [(2,2), (3,3), (4,4), (5,5)], [(3,2), (4,5), (5,5)])
             ]
,Delta_List: [-(1,1)
             ,(unknown [(2,2), (3,3), (4,4), (5,5)], [(1,1), (2,2), (3,2), (4,5), (5,5)])
             ]
,Delta_List: [+(1,1)
              ,(unknown [(1,1), (2,2), (3,3), (4,4), (5,5)], [(2,2), (3,2), (4,5), (5,5)])
             ]
,Delta_List: [<Delta_Pair: ("="1, "="1)>
             ,<Delta_Pair: ("="2, "="2)>
             ,<Delta_Pair: ("="3, (-3, +2))>
             ,<Delta_Pair: ("="4, (-4, +5))>
             ,(unknown (5,5), (5,5))
             ,(unknown [], [])
             ]
]
-- For the last Delta_List,
-- we don't allows the (unknown [] []) appear.
-- Since the weight of let dl = Delta_List [(Step_Delta (Unknown [], []))] = 0
-- lwb_Delta dl = NaN
-- we cannot to calculate the lwb and upb of empty properties.
-- Or we can add a line in Delta 214

--weight_Delta (Delta_List [(Step_Delta (Unknown (List EmptyList) (List EmptyList)))]) = 1


-}
extend12 = refine (head insert11)
insert12 = ins_cands (tail insert11) extend12

extend13 = refine (head insert12)
insert13 = ins_cands (tail insert12) extend13

extend14 = refine (head insert13)
insert14 = ins_cands (tail insert13) extend14

extend15 = refine (head insert14)
insert15 = ins_cands (tail insert14) extend15

extend16 = refine (head insert15)
insert16 = ins_cands (tail insert15) extend16

extend17 = refine (head insert16)
insert17 = ins_cands (tail insert16) extend17

extend18 = refine (head insert17)
insert18 = ins_cands (tail insert17) extend18

extend19 = refine (head insert18)
insert19 = ins_cands (tail insert18) extend19

extend20 = refine (head insert19)
insert20 = ins_cands (tail insert19) extend20

extend21 = refine (head insert20)
insert21 = ins_cands (tail insert20) extend21

extend22 = refine (head insert21)
insert22 = ins_cands (tail insert21) extend22

extend23 = refine (head insert22)
insert23 = ins_cands (tail insert22) extend23

extend24 = refine (head insert23)
insert24 = ins_cands (tail insert23) extend24

extend25 = refine (head insert24)
insert25 = ins_cands (tail insert24) extend25

extend26 = refine (head insert25)
insert26 = ins_cands (tail insert25) extend26

extend27 = refine (head insert26)
insert27 = ins_cands (tail insert26) extend27

extend28 = refine (head insert27)
insert28 = ins_cands (tail insert27) extend28

extend29 = refine (head insert28)
insert29 = ins_cands (tail insert28) extend29

extend30 = refine (head insert29)
insert30 = ins_cands (tail insert29) extend30

extend31 = refine (head insert30)
insert31 = ins_cands (tail insert30) extend31

extend32 = refine (head insert31)
insert32 = ins_cands (tail insert31) extend32

extend33 = refine (head insert32)
insert33 = ins_cands (tail insert32) extend33





