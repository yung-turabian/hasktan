{-# OPTIONS_GHC -w #-}
-- Henry Wandover
-- CMSC 305, Lab 4
-- Due: Friday, Nov. 8th 2024

module Grammar where
  
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,314) ([57344,4101,3331,0,2,0,0,0,4096,0,6144,2016,5,14,20480,0,0,31,0,0,0,0,0,0,0,0,0,0,0,0,0,6016,3136,52,1792,0,40,3584,0,80,7168,0,160,30720,50185,832,0,0,0,57344,0,1280,0,0,32768,0,0,16384,0,32815,26648,0,12288,12224,10,0,32768,0,56,16384,1,112,32768,2,0,0,0,4096,63494,321,896,0,20,1792,0,40,3584,0,80,7168,0,160,14336,0,320,0,0,0,57344,4101,3331,49152,8203,6662,32768,16407,13324,0,32815,26648,0,94,53297,0,188,41058,1,376,16580,3,752,33160,6,1504,784,13,3008,1568,26,0,16,0,12032,6272,104,0,49200,2575,0,32864,5151,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4032,0,0,8064,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12032,6272,104,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,49152,1,2560,0,0,0,0,32815,26648,0,12544,4032,10,24640,8064,20,376,16580,3,752,33160,6,0,64515,162,0,1,0,0,61452,643,0,57368,1287,448,0,16,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,28672,0,1024,0,512,4096,49152,1,4096,0,2048,0,0,7,16384,0,0,0,0,16384,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseHasquelito","Expr","Juxt","Atom","TypeExp","PrimType","List","ListMembers","Int","Bool","Float","int","float","bool","let","in","if","then","else","'\\\\'","\"->\"","\"::\"","\"&&\"","\"||\"","'='","'+'","'-'","'*'","'/'","'^'","quot","rem","\"==\"","\">=\"","\"<=\"","\"/=\"","'>'","'<'","'('","')'","var","'['","']'","':'","','","\"++\"","%eof"]
        bit_start = st * 49
        bit_end = (st + 1) * 49
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..48]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (14) = happyShift action_7
action_0 (15) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (17) = happyShift action_2
action_0 (19) = happyShift action_10
action_0 (29) = happyShift action_11
action_0 (33) = happyShift action_12
action_0 (34) = happyShift action_13
action_0 (41) = happyShift action_14
action_0 (43) = happyShift action_15
action_0 (44) = happyShift action_16
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (17) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (43) = happyShift action_42
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (25) = happyShift action_32
action_3 (26) = happyShift action_33
action_3 (35) = happyShift action_34
action_3 (36) = happyShift action_35
action_3 (37) = happyShift action_36
action_3 (38) = happyShift action_37
action_3 (39) = happyShift action_38
action_3 (40) = happyShift action_39
action_3 (46) = happyShift action_40
action_3 (48) = happyShift action_41
action_3 (49) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (14) = happyShift action_7
action_4 (15) = happyShift action_8
action_4 (16) = happyShift action_9
action_4 (41) = happyShift action_19
action_4 (43) = happyShift action_15
action_4 (6) = happyGoto action_31
action_4 _ = happyReduce_15

action_5 (28) = happyShift action_26
action_5 (29) = happyShift action_27
action_5 (30) = happyShift action_28
action_5 (31) = happyShift action_29
action_5 (32) = happyShift action_30
action_5 _ = happyReduce_25

action_6 _ = happyReduce_14

action_7 _ = happyReduce_27

action_8 _ = happyReduce_29

action_9 _ = happyReduce_28

action_10 (14) = happyShift action_7
action_10 (15) = happyShift action_8
action_10 (16) = happyShift action_9
action_10 (17) = happyShift action_2
action_10 (19) = happyShift action_10
action_10 (29) = happyShift action_11
action_10 (33) = happyShift action_12
action_10 (34) = happyShift action_13
action_10 (41) = happyShift action_14
action_10 (43) = happyShift action_15
action_10 (44) = happyShift action_16
action_10 (4) = happyGoto action_25
action_10 (5) = happyGoto action_4
action_10 (6) = happyGoto action_5
action_10 (9) = happyGoto action_6
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (14) = happyShift action_7
action_11 (15) = happyShift action_8
action_11 (16) = happyShift action_9
action_11 (41) = happyShift action_19
action_11 (43) = happyShift action_15
action_11 (6) = happyGoto action_24
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (14) = happyShift action_7
action_12 (15) = happyShift action_8
action_12 (16) = happyShift action_9
action_12 (41) = happyShift action_19
action_12 (43) = happyShift action_15
action_12 (6) = happyGoto action_23
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (14) = happyShift action_7
action_13 (15) = happyShift action_8
action_13 (16) = happyShift action_9
action_13 (41) = happyShift action_19
action_13 (43) = happyShift action_15
action_13 (6) = happyGoto action_22
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (14) = happyShift action_7
action_14 (15) = happyShift action_8
action_14 (16) = happyShift action_9
action_14 (17) = happyShift action_2
action_14 (19) = happyShift action_10
action_14 (22) = happyShift action_21
action_14 (29) = happyShift action_11
action_14 (33) = happyShift action_12
action_14 (34) = happyShift action_13
action_14 (41) = happyShift action_14
action_14 (43) = happyShift action_15
action_14 (44) = happyShift action_16
action_14 (4) = happyGoto action_20
action_14 (5) = happyGoto action_4
action_14 (6) = happyGoto action_5
action_14 (9) = happyGoto action_6
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_30

action_16 (14) = happyShift action_7
action_16 (15) = happyShift action_8
action_16 (16) = happyShift action_9
action_16 (41) = happyShift action_19
action_16 (43) = happyShift action_15
action_16 (6) = happyGoto action_17
action_16 (10) = happyGoto action_18
action_16 _ = happyReduce_38

action_17 (47) = happyShift action_65
action_17 _ = happyReduce_39

action_18 (45) = happyShift action_64
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_7
action_19 (15) = happyShift action_8
action_19 (16) = happyShift action_9
action_19 (17) = happyShift action_2
action_19 (19) = happyShift action_10
action_19 (29) = happyShift action_11
action_19 (33) = happyShift action_12
action_19 (34) = happyShift action_13
action_19 (41) = happyShift action_14
action_19 (43) = happyShift action_15
action_19 (44) = happyShift action_16
action_19 (4) = happyGoto action_20
action_19 (5) = happyGoto action_4
action_19 (6) = happyGoto action_5
action_19 (9) = happyGoto action_6
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (25) = happyShift action_32
action_20 (26) = happyShift action_33
action_20 (35) = happyShift action_34
action_20 (36) = happyShift action_35
action_20 (37) = happyShift action_36
action_20 (38) = happyShift action_37
action_20 (39) = happyShift action_38
action_20 (40) = happyShift action_39
action_20 (42) = happyShift action_63
action_20 (46) = happyShift action_40
action_20 (48) = happyShift action_41
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (43) = happyShift action_62
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_7
action_22 (15) = happyShift action_8
action_22 (16) = happyShift action_9
action_22 (41) = happyShift action_19
action_22 (43) = happyShift action_15
action_22 (6) = happyGoto action_61
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_7
action_23 (15) = happyShift action_8
action_23 (16) = happyShift action_9
action_23 (41) = happyShift action_19
action_23 (43) = happyShift action_15
action_23 (6) = happyGoto action_60
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_19

action_25 (20) = happyShift action_59
action_25 (25) = happyShift action_32
action_25 (26) = happyShift action_33
action_25 (35) = happyShift action_34
action_25 (36) = happyShift action_35
action_25 (37) = happyShift action_36
action_25 (38) = happyShift action_37
action_25 (39) = happyShift action_38
action_25 (40) = happyShift action_39
action_25 (46) = happyShift action_40
action_25 (48) = happyShift action_41
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_7
action_26 (15) = happyShift action_8
action_26 (16) = happyShift action_9
action_26 (41) = happyShift action_19
action_26 (43) = happyShift action_15
action_26 (6) = happyGoto action_58
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (14) = happyShift action_7
action_27 (15) = happyShift action_8
action_27 (16) = happyShift action_9
action_27 (41) = happyShift action_19
action_27 (43) = happyShift action_15
action_27 (6) = happyGoto action_57
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (14) = happyShift action_7
action_28 (15) = happyShift action_8
action_28 (16) = happyShift action_9
action_28 (41) = happyShift action_19
action_28 (43) = happyShift action_15
action_28 (6) = happyGoto action_56
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (14) = happyShift action_7
action_29 (15) = happyShift action_8
action_29 (16) = happyShift action_9
action_29 (41) = happyShift action_19
action_29 (43) = happyShift action_15
action_29 (6) = happyGoto action_55
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (14) = happyShift action_7
action_30 (15) = happyShift action_8
action_30 (16) = happyShift action_9
action_30 (41) = happyShift action_19
action_30 (43) = happyShift action_15
action_30 (6) = happyGoto action_54
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_16

action_32 (14) = happyShift action_7
action_32 (15) = happyShift action_8
action_32 (16) = happyShift action_9
action_32 (17) = happyShift action_2
action_32 (19) = happyShift action_10
action_32 (29) = happyShift action_11
action_32 (33) = happyShift action_12
action_32 (34) = happyShift action_13
action_32 (41) = happyShift action_14
action_32 (43) = happyShift action_15
action_32 (44) = happyShift action_16
action_32 (4) = happyGoto action_53
action_32 (5) = happyGoto action_4
action_32 (6) = happyGoto action_5
action_32 (9) = happyGoto action_6
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (14) = happyShift action_7
action_33 (15) = happyShift action_8
action_33 (16) = happyShift action_9
action_33 (17) = happyShift action_2
action_33 (19) = happyShift action_10
action_33 (29) = happyShift action_11
action_33 (33) = happyShift action_12
action_33 (34) = happyShift action_13
action_33 (41) = happyShift action_14
action_33 (43) = happyShift action_15
action_33 (44) = happyShift action_16
action_33 (4) = happyGoto action_52
action_33 (5) = happyGoto action_4
action_33 (6) = happyGoto action_5
action_33 (9) = happyGoto action_6
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (14) = happyShift action_7
action_34 (15) = happyShift action_8
action_34 (16) = happyShift action_9
action_34 (17) = happyShift action_2
action_34 (19) = happyShift action_10
action_34 (29) = happyShift action_11
action_34 (33) = happyShift action_12
action_34 (34) = happyShift action_13
action_34 (41) = happyShift action_14
action_34 (43) = happyShift action_15
action_34 (44) = happyShift action_16
action_34 (4) = happyGoto action_51
action_34 (5) = happyGoto action_4
action_34 (6) = happyGoto action_5
action_34 (9) = happyGoto action_6
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (14) = happyShift action_7
action_35 (15) = happyShift action_8
action_35 (16) = happyShift action_9
action_35 (17) = happyShift action_2
action_35 (19) = happyShift action_10
action_35 (29) = happyShift action_11
action_35 (33) = happyShift action_12
action_35 (34) = happyShift action_13
action_35 (41) = happyShift action_14
action_35 (43) = happyShift action_15
action_35 (44) = happyShift action_16
action_35 (4) = happyGoto action_50
action_35 (5) = happyGoto action_4
action_35 (6) = happyGoto action_5
action_35 (9) = happyGoto action_6
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (14) = happyShift action_7
action_36 (15) = happyShift action_8
action_36 (16) = happyShift action_9
action_36 (17) = happyShift action_2
action_36 (19) = happyShift action_10
action_36 (29) = happyShift action_11
action_36 (33) = happyShift action_12
action_36 (34) = happyShift action_13
action_36 (41) = happyShift action_14
action_36 (43) = happyShift action_15
action_36 (44) = happyShift action_16
action_36 (4) = happyGoto action_49
action_36 (5) = happyGoto action_4
action_36 (6) = happyGoto action_5
action_36 (9) = happyGoto action_6
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (14) = happyShift action_7
action_37 (15) = happyShift action_8
action_37 (16) = happyShift action_9
action_37 (17) = happyShift action_2
action_37 (19) = happyShift action_10
action_37 (29) = happyShift action_11
action_37 (33) = happyShift action_12
action_37 (34) = happyShift action_13
action_37 (41) = happyShift action_14
action_37 (43) = happyShift action_15
action_37 (44) = happyShift action_16
action_37 (4) = happyGoto action_48
action_37 (5) = happyGoto action_4
action_37 (6) = happyGoto action_5
action_37 (9) = happyGoto action_6
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (14) = happyShift action_7
action_38 (15) = happyShift action_8
action_38 (16) = happyShift action_9
action_38 (17) = happyShift action_2
action_38 (19) = happyShift action_10
action_38 (29) = happyShift action_11
action_38 (33) = happyShift action_12
action_38 (34) = happyShift action_13
action_38 (41) = happyShift action_14
action_38 (43) = happyShift action_15
action_38 (44) = happyShift action_16
action_38 (4) = happyGoto action_47
action_38 (5) = happyGoto action_4
action_38 (6) = happyGoto action_5
action_38 (9) = happyGoto action_6
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (14) = happyShift action_7
action_39 (15) = happyShift action_8
action_39 (16) = happyShift action_9
action_39 (17) = happyShift action_2
action_39 (19) = happyShift action_10
action_39 (29) = happyShift action_11
action_39 (33) = happyShift action_12
action_39 (34) = happyShift action_13
action_39 (41) = happyShift action_14
action_39 (43) = happyShift action_15
action_39 (44) = happyShift action_16
action_39 (4) = happyGoto action_46
action_39 (5) = happyGoto action_4
action_39 (6) = happyGoto action_5
action_39 (9) = happyGoto action_6
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (14) = happyShift action_7
action_40 (15) = happyShift action_8
action_40 (16) = happyShift action_9
action_40 (17) = happyShift action_2
action_40 (19) = happyShift action_10
action_40 (29) = happyShift action_11
action_40 (33) = happyShift action_12
action_40 (34) = happyShift action_13
action_40 (41) = happyShift action_14
action_40 (43) = happyShift action_15
action_40 (44) = happyShift action_16
action_40 (4) = happyGoto action_45
action_40 (5) = happyGoto action_4
action_40 (6) = happyGoto action_5
action_40 (9) = happyGoto action_6
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (14) = happyShift action_7
action_41 (15) = happyShift action_8
action_41 (16) = happyShift action_9
action_41 (17) = happyShift action_2
action_41 (19) = happyShift action_10
action_41 (29) = happyShift action_11
action_41 (33) = happyShift action_12
action_41 (34) = happyShift action_13
action_41 (41) = happyShift action_14
action_41 (43) = happyShift action_15
action_41 (44) = happyShift action_16
action_41 (4) = happyGoto action_44
action_41 (5) = happyGoto action_4
action_41 (6) = happyGoto action_5
action_41 (9) = happyGoto action_6
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (27) = happyShift action_43
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (14) = happyShift action_7
action_43 (15) = happyShift action_8
action_43 (16) = happyShift action_9
action_43 (17) = happyShift action_2
action_43 (19) = happyShift action_10
action_43 (29) = happyShift action_11
action_43 (33) = happyShift action_12
action_43 (34) = happyShift action_13
action_43 (41) = happyShift action_14
action_43 (43) = happyShift action_15
action_43 (44) = happyShift action_16
action_43 (4) = happyGoto action_69
action_43 (5) = happyGoto action_4
action_43 (6) = happyGoto action_5
action_43 (9) = happyGoto action_6
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (25) = happyShift action_32
action_44 (26) = happyShift action_33
action_44 (35) = happyShift action_34
action_44 (36) = happyShift action_35
action_44 (37) = happyShift action_36
action_44 (38) = happyShift action_37
action_44 (39) = happyShift action_38
action_44 (40) = happyShift action_39
action_44 (46) = happyShift action_40
action_44 (48) = happyShift action_41
action_44 _ = happyReduce_13

action_45 (25) = happyShift action_32
action_45 (26) = happyShift action_33
action_45 (35) = happyShift action_34
action_45 (36) = happyShift action_35
action_45 (37) = happyShift action_36
action_45 (38) = happyShift action_37
action_45 (39) = happyShift action_38
action_45 (40) = happyShift action_39
action_45 (46) = happyShift action_40
action_45 (48) = happyShift action_41
action_45 _ = happyReduce_12

action_46 (35) = happyFail []
action_46 (36) = happyFail []
action_46 (37) = happyFail []
action_46 (38) = happyFail []
action_46 (39) = happyFail []
action_46 (40) = happyFail []
action_46 _ = happyReduce_9

action_47 (35) = happyFail []
action_47 (36) = happyFail []
action_47 (37) = happyFail []
action_47 (38) = happyFail []
action_47 (39) = happyFail []
action_47 (40) = happyFail []
action_47 _ = happyReduce_8

action_48 (35) = happyFail []
action_48 (36) = happyFail []
action_48 (37) = happyFail []
action_48 (38) = happyFail []
action_48 (39) = happyFail []
action_48 (40) = happyFail []
action_48 _ = happyReduce_7

action_49 (35) = happyFail []
action_49 (36) = happyFail []
action_49 (37) = happyFail []
action_49 (38) = happyFail []
action_49 (39) = happyFail []
action_49 (40) = happyFail []
action_49 _ = happyReduce_6

action_50 (35) = happyFail []
action_50 (36) = happyFail []
action_50 (37) = happyFail []
action_50 (38) = happyFail []
action_50 (39) = happyFail []
action_50 (40) = happyFail []
action_50 _ = happyReduce_5

action_51 (35) = happyFail []
action_51 (36) = happyFail []
action_51 (37) = happyFail []
action_51 (38) = happyFail []
action_51 (39) = happyFail []
action_51 (40) = happyFail []
action_51 _ = happyReduce_4

action_52 (25) = happyFail []
action_52 (26) = happyFail []
action_52 (35) = happyShift action_34
action_52 (36) = happyShift action_35
action_52 (37) = happyShift action_36
action_52 (38) = happyShift action_37
action_52 (39) = happyShift action_38
action_52 (40) = happyShift action_39
action_52 _ = happyReduce_11

action_53 (25) = happyFail []
action_53 (26) = happyFail []
action_53 (35) = happyShift action_34
action_53 (36) = happyShift action_35
action_53 (37) = happyShift action_36
action_53 (38) = happyShift action_37
action_53 (39) = happyShift action_38
action_53 (40) = happyShift action_39
action_53 _ = happyReduce_10

action_54 _ = happyReduce_24

action_55 _ = happyReduce_23

action_56 _ = happyReduce_22

action_57 _ = happyReduce_21

action_58 _ = happyReduce_20

action_59 (14) = happyShift action_7
action_59 (15) = happyShift action_8
action_59 (16) = happyShift action_9
action_59 (17) = happyShift action_2
action_59 (19) = happyShift action_10
action_59 (29) = happyShift action_11
action_59 (33) = happyShift action_12
action_59 (34) = happyShift action_13
action_59 (41) = happyShift action_14
action_59 (43) = happyShift action_15
action_59 (44) = happyShift action_16
action_59 (4) = happyGoto action_68
action_59 (5) = happyGoto action_4
action_59 (6) = happyGoto action_5
action_59 (9) = happyGoto action_6
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_17

action_61 _ = happyReduce_18

action_62 (23) = happyShift action_67
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_26

action_64 _ = happyReduce_37

action_65 (14) = happyShift action_7
action_65 (15) = happyShift action_8
action_65 (16) = happyShift action_9
action_65 (41) = happyShift action_19
action_65 (43) = happyShift action_15
action_65 (6) = happyGoto action_17
action_65 (10) = happyGoto action_66
action_65 _ = happyReduce_38

action_66 _ = happyReduce_40

action_67 (14) = happyShift action_7
action_67 (15) = happyShift action_8
action_67 (16) = happyShift action_9
action_67 (17) = happyShift action_2
action_67 (19) = happyShift action_10
action_67 (29) = happyShift action_11
action_67 (33) = happyShift action_12
action_67 (34) = happyShift action_13
action_67 (41) = happyShift action_14
action_67 (43) = happyShift action_15
action_67 (44) = happyShift action_16
action_67 (4) = happyGoto action_72
action_67 (5) = happyGoto action_4
action_67 (6) = happyGoto action_5
action_67 (9) = happyGoto action_6
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (21) = happyShift action_71
action_68 (25) = happyShift action_32
action_68 (26) = happyShift action_33
action_68 (35) = happyShift action_34
action_68 (36) = happyShift action_35
action_68 (37) = happyShift action_36
action_68 (38) = happyShift action_37
action_68 (39) = happyShift action_38
action_68 (40) = happyShift action_39
action_68 (46) = happyShift action_40
action_68 (48) = happyShift action_41
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (18) = happyShift action_70
action_69 (25) = happyShift action_32
action_69 (26) = happyShift action_33
action_69 (35) = happyShift action_34
action_69 (36) = happyShift action_35
action_69 (37) = happyShift action_36
action_69 (38) = happyShift action_37
action_69 (39) = happyShift action_38
action_69 (40) = happyShift action_39
action_69 (46) = happyShift action_40
action_69 (48) = happyShift action_41
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (14) = happyShift action_7
action_70 (15) = happyShift action_8
action_70 (16) = happyShift action_9
action_70 (17) = happyShift action_2
action_70 (19) = happyShift action_10
action_70 (29) = happyShift action_11
action_70 (33) = happyShift action_12
action_70 (34) = happyShift action_13
action_70 (41) = happyShift action_14
action_70 (43) = happyShift action_15
action_70 (44) = happyShift action_16
action_70 (4) = happyGoto action_75
action_70 (5) = happyGoto action_4
action_70 (6) = happyGoto action_5
action_70 (9) = happyGoto action_6
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (14) = happyShift action_7
action_71 (15) = happyShift action_8
action_71 (16) = happyShift action_9
action_71 (17) = happyShift action_2
action_71 (19) = happyShift action_10
action_71 (29) = happyShift action_11
action_71 (33) = happyShift action_12
action_71 (34) = happyShift action_13
action_71 (41) = happyShift action_14
action_71 (43) = happyShift action_15
action_71 (44) = happyShift action_16
action_71 (4) = happyGoto action_74
action_71 (5) = happyGoto action_4
action_71 (6) = happyGoto action_5
action_71 (9) = happyGoto action_6
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (25) = happyShift action_32
action_72 (26) = happyShift action_33
action_72 (35) = happyShift action_34
action_72 (36) = happyShift action_35
action_72 (37) = happyShift action_36
action_72 (38) = happyShift action_37
action_72 (39) = happyShift action_38
action_72 (40) = happyShift action_39
action_72 (42) = happyShift action_73
action_72 (46) = happyShift action_40
action_72 (48) = happyShift action_41
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (24) = happyShift action_76
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (25) = happyShift action_32
action_74 (26) = happyShift action_33
action_74 (35) = happyShift action_34
action_74 (36) = happyShift action_35
action_74 (37) = happyShift action_36
action_74 (38) = happyShift action_37
action_74 (39) = happyShift action_38
action_74 (40) = happyShift action_39
action_74 (46) = happyShift action_40
action_74 (48) = happyShift action_41
action_74 _ = happyReduce_3

action_75 (25) = happyShift action_32
action_75 (26) = happyShift action_33
action_75 (35) = happyShift action_34
action_75 (36) = happyShift action_35
action_75 (37) = happyShift action_36
action_75 (38) = happyShift action_37
action_75 (39) = happyShift action_38
action_75 (40) = happyShift action_39
action_75 (46) = happyShift action_40
action_75 (48) = happyShift action_41
action_75 _ = happyReduce_1

action_76 (11) = happyShift action_79
action_76 (12) = happyShift action_80
action_76 (13) = happyShift action_81
action_76 (41) = happyShift action_82
action_76 (7) = happyGoto action_77
action_76 (8) = happyGoto action_78
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (23) = happyShift action_84
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_31

action_79 _ = happyReduce_35

action_80 _ = happyReduce_34

action_81 _ = happyReduce_36

action_82 (11) = happyShift action_79
action_82 (12) = happyShift action_80
action_82 (13) = happyShift action_81
action_82 (41) = happyShift action_82
action_82 (7) = happyGoto action_83
action_82 (8) = happyGoto action_78
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (23) = happyShift action_86
action_83 (42) = happyShift action_87
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (11) = happyShift action_79
action_84 (12) = happyShift action_80
action_84 (13) = happyShift action_81
action_84 (41) = happyShift action_82
action_84 (7) = happyGoto action_85
action_84 (8) = happyGoto action_78
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (23) = happyShift action_86
action_85 _ = happyReduce_2

action_86 (11) = happyShift action_79
action_86 (12) = happyShift action_80
action_86 (13) = happyShift action_81
action_86 (41) = happyShift action_82
action_86 (7) = happyGoto action_88
action_86 (8) = happyGoto action_78
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_33

action_88 (23) = happyShift action_86
action_88 _ = happyReduce_32

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (VAR p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 10 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn7  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (VAR p happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lambda happy_var_3 happy_var_5 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 6 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Equals happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (Equals happy_var_1 happy_var_3) (Gt happy_var_1 happy_var_3)
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (Equals happy_var_1 happy_var_3) (Lt happy_var_1 happy_var_3)
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App (Not) (Equals happy_var_1 happy_var_3)
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Cons happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Concat happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  4 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  4 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  5 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (App happy_var_1 happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Quot happy_var_2 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Rem happy_var_2 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  5 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Minus (Integer 0) happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  5 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  5 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Times happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  5 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Divide happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  5 happyReduction_24
happyReduction_24 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Power happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  5 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  6 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  6 happyReduction_27
happyReduction_27 (HappyTerminal (INTVAL p happy_var_1))
	 =  HappyAbsSyn6
		 (Integer happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  6 happyReduction_28
happyReduction_28 (HappyTerminal (BOOLVAL p happy_var_1))
	 =  HappyAbsSyn6
		 (Boolean happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  6 happyReduction_29
happyReduction_29 (HappyTerminal (FLOATVAL p happy_var_1))
	 =  HappyAbsSyn6
		 (Float happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  6 happyReduction_30
happyReduction_30 (HappyTerminal (VAR p happy_var_1))
	 =  HappyAbsSyn6
		 (Variable happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  7 happyReduction_31
happyReduction_31 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  7 happyReduction_32
happyReduction_32 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  7 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  8 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn8
		 (BoolType
	)

happyReduce_35 = happySpecReduce_1  8 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn8
		 (IntType
	)

happyReduce_36 = happySpecReduce_1  8 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn8
		 (FloatType
	)

happyReduce_37 = happySpecReduce_3  9 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (List happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  10 happyReduction_38
happyReduction_38  =  HappyAbsSyn10
		 ([]
	)

happyReduce_39 = happySpecReduce_1  10 happyReduction_39
happyReduction_39 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  10 happyReduction_40
happyReduction_40 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 49 49 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	INT p -> cont 11;
	BOOL p -> cont 12;
	FLOAT p -> cont 13;
	INTVAL p happy_dollar_dollar -> cont 14;
	FLOATVAL p happy_dollar_dollar -> cont 15;
	BOOLVAL p happy_dollar_dollar -> cont 16;
	LET p -> cont 17;
	IN p -> cont 18;
	IF p -> cont 19;
	THEN p -> cont 20;
	ELSE p -> cont 21;
	LAMBDA p -> cont 22;
	ARROW p -> cont 23;
	COLONS p -> cont 24;
	AND p -> cont 25;
	OR p -> cont 26;
	BIND p -> cont 27;
	PLUS p -> cont 28;
	MINUS p -> cont 29;
	TIMES p -> cont 30;
	DIVIDE p -> cont 31;
	POWER p -> cont 32;
	QUOT p -> cont 33;
	REM p -> cont 34;
	EQUALS p -> cont 35;
	GTEQ p -> cont 36;
	LTEQ p -> cont 37;
	NOTEQ p -> cont 38;
	GT' p -> cont 39;
	LT' p -> cont 40;
	LPAREN p -> cont 41;
	RPAREN p -> cont 42;
	VAR p happy_dollar_dollar -> cont 43;
	LBRACK p -> cont 44;
	RBRACK p -> cont 45;
	COLON p -> cont 46;
	COMMA p -> cont 47;
	PLUSPLUS p -> cont 48;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 49 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => ([(Token)], [String]) -> E a
happyError' = (\(tokens, _) -> parseError tokens)
parseHasquelito tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data E a = Ok a | Failed String 
 deriving(Eq,Show)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of
    Ok a      -> k a
    Failed e  -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
  case m of
    Ok a      -> Ok a
    Failed e  -> k e

parseError :: [Token] -> E a
parseError tokens = failE "Parse Error"


data TypeExp
  = BoolType
  | IntType
  | FloatType
   
	| EmptyList
  | IntList
	| BoolList
	| FloatList

  | Arrow TypeExp TypeExp
  deriving (Eq,Ord,Show)

data AST
  = Boolean Bool
  | Integer Int
  | Float Float
  
  | Let String AST AST
  | If AST AST AST

  | Lambda String AST TypeExp TypeExp

  | App AST AST

  | And AST AST
  | Or AST AST
  
  | Plus AST AST
  | Minus AST AST
  | Times AST AST
  | Divide AST AST
	| Power AST AST

  | Quot AST AST
  | Rem AST AST

  | Equals AST AST
  | Lt AST AST
  | Gt AST AST

  | Variable String

  | List [AST]
  | Cons AST AST
  | Concat AST AST

  | Not -- Using the haskell `not`, this is simply a placeholder.
  deriving (Eq,Ord,Show)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.