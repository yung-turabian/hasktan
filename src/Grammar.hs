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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,385) ([49152,8203,6662,0,8,0,0,0,32768,0,32768,32257,80,0,496,0,1792,0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,752,33160,6,448,0,10,1792,0,40,7168,0,160,61440,34834,1665,0,0,0,0,7,10240,0,0,0,8,0,0,8,3008,1568,26,0,57368,1303,0,0,128,28672,0,640,49152,1,2560,0,0,0,0,24832,8064,20,0,0,0,448,1568,10,1792,6272,40,7168,25088,160,28672,34816,641,49152,8193,2566,0,32815,26648,0,188,41058,1,752,33160,6,3008,1568,26,12032,6272,104,48128,25088,416,61440,34818,1665,49152,8203,6662,0,32815,26648,0,188,41058,1,0,2,0,3008,1568,26,0,57368,1287,0,32864,5151,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,7,0,32768,31,0,0,0,0,0,1,0,0,4,0,0,28,0,0,112,0,3008,1568,26,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,28,40960,0,0,0,0,3008,1568,26,32768,57368,1287,16384,32864,5151,61440,34818,1665,49152,8203,6662,0,6144,6112,5,4096,0,0,32768,32257,80,0,63494,321,224,0,72,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14,32768,4,56,0,50,0,2,128,0,0,0,0,32,256,14336,0,4608,0,512,0,32768,3,8192,1,0,0,0,0,0,0,0,2,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseHasquelito","Expr","Form","Juxt","Atom","TypeExp","PrimType","List","ListMembers","Int","Bool","Float","int","float","bool","let","in","if","then","else","'\\\\'","\"->\"","\"::\"","\"&&\"","\"||\"","'='","'+'","'-'","'*'","'/'","'^'","quot","rem","\"==\"","\">=\"","\"<=\"","\"/=\"","'>'","'<'","'('","')'","var","'['","']'","':'","','","\"++\"","%eof"]
        bit_start = st * 50
        bit_end = (st + 1) * 50
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..49]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (15) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (17) = happyShift action_10
action_0 (18) = happyShift action_2
action_0 (20) = happyShift action_11
action_0 (30) = happyShift action_12
action_0 (34) = happyShift action_13
action_0 (35) = happyShift action_14
action_0 (42) = happyShift action_15
action_0 (44) = happyShift action_16
action_0 (45) = happyShift action_17
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (18) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (44) = happyShift action_43
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (26) = happyShift action_33
action_3 (27) = happyShift action_34
action_3 (36) = happyShift action_35
action_3 (37) = happyShift action_36
action_3 (38) = happyShift action_37
action_3 (39) = happyShift action_38
action_3 (40) = happyShift action_39
action_3 (41) = happyShift action_40
action_3 (47) = happyShift action_41
action_3 (49) = happyShift action_42
action_3 (50) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (29) = happyShift action_28
action_4 (30) = happyShift action_29
action_4 (31) = happyShift action_30
action_4 (32) = happyShift action_31
action_4 (33) = happyShift action_32
action_4 _ = happyReduce_15

action_5 (15) = happyShift action_8
action_5 (16) = happyShift action_9
action_5 (17) = happyShift action_10
action_5 (42) = happyShift action_20
action_5 (44) = happyShift action_16
action_5 (7) = happyGoto action_27
action_5 _ = happyReduce_21

action_6 _ = happyReduce_26

action_7 _ = happyReduce_14

action_8 _ = happyReduce_28

action_9 _ = happyReduce_30

action_10 _ = happyReduce_29

action_11 (15) = happyShift action_8
action_11 (16) = happyShift action_9
action_11 (17) = happyShift action_10
action_11 (18) = happyShift action_2
action_11 (20) = happyShift action_11
action_11 (30) = happyShift action_12
action_11 (34) = happyShift action_13
action_11 (35) = happyShift action_14
action_11 (42) = happyShift action_15
action_11 (44) = happyShift action_16
action_11 (45) = happyShift action_17
action_11 (4) = happyGoto action_26
action_11 (5) = happyGoto action_4
action_11 (6) = happyGoto action_5
action_11 (7) = happyGoto action_6
action_11 (10) = happyGoto action_7
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (15) = happyShift action_8
action_12 (16) = happyShift action_9
action_12 (17) = happyShift action_10
action_12 (42) = happyShift action_20
action_12 (44) = happyShift action_16
action_12 (7) = happyGoto action_25
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_8
action_13 (16) = happyShift action_9
action_13 (17) = happyShift action_10
action_13 (42) = happyShift action_20
action_13 (44) = happyShift action_16
action_13 (7) = happyGoto action_24
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (15) = happyShift action_8
action_14 (16) = happyShift action_9
action_14 (17) = happyShift action_10
action_14 (42) = happyShift action_20
action_14 (44) = happyShift action_16
action_14 (7) = happyGoto action_23
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_8
action_15 (16) = happyShift action_9
action_15 (17) = happyShift action_10
action_15 (18) = happyShift action_2
action_15 (20) = happyShift action_11
action_15 (23) = happyShift action_22
action_15 (30) = happyShift action_12
action_15 (34) = happyShift action_13
action_15 (35) = happyShift action_14
action_15 (42) = happyShift action_15
action_15 (44) = happyShift action_16
action_15 (45) = happyShift action_17
action_15 (4) = happyGoto action_21
action_15 (5) = happyGoto action_4
action_15 (6) = happyGoto action_5
action_15 (7) = happyGoto action_6
action_15 (10) = happyGoto action_7
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_31

action_17 (15) = happyShift action_8
action_17 (16) = happyShift action_9
action_17 (17) = happyShift action_10
action_17 (42) = happyShift action_20
action_17 (44) = happyShift action_16
action_17 (7) = happyGoto action_18
action_17 (11) = happyGoto action_19
action_17 _ = happyReduce_41

action_18 (48) = happyShift action_66
action_18 _ = happyReduce_42

action_19 (46) = happyShift action_65
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (15) = happyShift action_8
action_20 (16) = happyShift action_9
action_20 (17) = happyShift action_10
action_20 (18) = happyShift action_2
action_20 (20) = happyShift action_11
action_20 (30) = happyShift action_12
action_20 (34) = happyShift action_13
action_20 (35) = happyShift action_14
action_20 (42) = happyShift action_15
action_20 (44) = happyShift action_16
action_20 (45) = happyShift action_17
action_20 (4) = happyGoto action_21
action_20 (5) = happyGoto action_4
action_20 (6) = happyGoto action_5
action_20 (7) = happyGoto action_6
action_20 (10) = happyGoto action_7
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (26) = happyShift action_33
action_21 (27) = happyShift action_34
action_21 (36) = happyShift action_35
action_21 (37) = happyShift action_36
action_21 (38) = happyShift action_37
action_21 (39) = happyShift action_38
action_21 (40) = happyShift action_39
action_21 (41) = happyShift action_40
action_21 (43) = happyShift action_64
action_21 (47) = happyShift action_41
action_21 (49) = happyShift action_42
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (44) = happyShift action_63
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (15) = happyShift action_8
action_23 (16) = happyShift action_9
action_23 (17) = happyShift action_10
action_23 (42) = happyShift action_20
action_23 (44) = happyShift action_16
action_23 (7) = happyGoto action_62
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (15) = happyShift action_8
action_24 (16) = happyShift action_9
action_24 (17) = happyShift action_10
action_24 (42) = happyShift action_20
action_24 (44) = happyShift action_16
action_24 (7) = happyGoto action_61
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_25

action_26 (21) = happyShift action_60
action_26 (26) = happyShift action_33
action_26 (27) = happyShift action_34
action_26 (36) = happyShift action_35
action_26 (37) = happyShift action_36
action_26 (38) = happyShift action_37
action_26 (39) = happyShift action_38
action_26 (40) = happyShift action_39
action_26 (41) = happyShift action_40
action_26 (47) = happyShift action_41
action_26 (49) = happyShift action_42
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_22

action_28 (15) = happyShift action_8
action_28 (16) = happyShift action_9
action_28 (17) = happyShift action_10
action_28 (30) = happyShift action_12
action_28 (34) = happyShift action_13
action_28 (35) = happyShift action_14
action_28 (42) = happyShift action_20
action_28 (44) = happyShift action_16
action_28 (5) = happyGoto action_59
action_28 (6) = happyGoto action_5
action_28 (7) = happyGoto action_6
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (15) = happyShift action_8
action_29 (16) = happyShift action_9
action_29 (17) = happyShift action_10
action_29 (30) = happyShift action_12
action_29 (34) = happyShift action_13
action_29 (35) = happyShift action_14
action_29 (42) = happyShift action_20
action_29 (44) = happyShift action_16
action_29 (5) = happyGoto action_58
action_29 (6) = happyGoto action_5
action_29 (7) = happyGoto action_6
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (15) = happyShift action_8
action_30 (16) = happyShift action_9
action_30 (17) = happyShift action_10
action_30 (30) = happyShift action_12
action_30 (34) = happyShift action_13
action_30 (35) = happyShift action_14
action_30 (42) = happyShift action_20
action_30 (44) = happyShift action_16
action_30 (5) = happyGoto action_57
action_30 (6) = happyGoto action_5
action_30 (7) = happyGoto action_6
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (15) = happyShift action_8
action_31 (16) = happyShift action_9
action_31 (17) = happyShift action_10
action_31 (30) = happyShift action_12
action_31 (34) = happyShift action_13
action_31 (35) = happyShift action_14
action_31 (42) = happyShift action_20
action_31 (44) = happyShift action_16
action_31 (5) = happyGoto action_56
action_31 (6) = happyGoto action_5
action_31 (7) = happyGoto action_6
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (15) = happyShift action_8
action_32 (16) = happyShift action_9
action_32 (17) = happyShift action_10
action_32 (30) = happyShift action_12
action_32 (34) = happyShift action_13
action_32 (35) = happyShift action_14
action_32 (42) = happyShift action_20
action_32 (44) = happyShift action_16
action_32 (5) = happyGoto action_55
action_32 (6) = happyGoto action_5
action_32 (7) = happyGoto action_6
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (15) = happyShift action_8
action_33 (16) = happyShift action_9
action_33 (17) = happyShift action_10
action_33 (18) = happyShift action_2
action_33 (20) = happyShift action_11
action_33 (30) = happyShift action_12
action_33 (34) = happyShift action_13
action_33 (35) = happyShift action_14
action_33 (42) = happyShift action_15
action_33 (44) = happyShift action_16
action_33 (45) = happyShift action_17
action_33 (4) = happyGoto action_54
action_33 (5) = happyGoto action_4
action_33 (6) = happyGoto action_5
action_33 (7) = happyGoto action_6
action_33 (10) = happyGoto action_7
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (15) = happyShift action_8
action_34 (16) = happyShift action_9
action_34 (17) = happyShift action_10
action_34 (18) = happyShift action_2
action_34 (20) = happyShift action_11
action_34 (30) = happyShift action_12
action_34 (34) = happyShift action_13
action_34 (35) = happyShift action_14
action_34 (42) = happyShift action_15
action_34 (44) = happyShift action_16
action_34 (45) = happyShift action_17
action_34 (4) = happyGoto action_53
action_34 (5) = happyGoto action_4
action_34 (6) = happyGoto action_5
action_34 (7) = happyGoto action_6
action_34 (10) = happyGoto action_7
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (15) = happyShift action_8
action_35 (16) = happyShift action_9
action_35 (17) = happyShift action_10
action_35 (18) = happyShift action_2
action_35 (20) = happyShift action_11
action_35 (30) = happyShift action_12
action_35 (34) = happyShift action_13
action_35 (35) = happyShift action_14
action_35 (42) = happyShift action_15
action_35 (44) = happyShift action_16
action_35 (45) = happyShift action_17
action_35 (4) = happyGoto action_52
action_35 (5) = happyGoto action_4
action_35 (6) = happyGoto action_5
action_35 (7) = happyGoto action_6
action_35 (10) = happyGoto action_7
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (15) = happyShift action_8
action_36 (16) = happyShift action_9
action_36 (17) = happyShift action_10
action_36 (18) = happyShift action_2
action_36 (20) = happyShift action_11
action_36 (30) = happyShift action_12
action_36 (34) = happyShift action_13
action_36 (35) = happyShift action_14
action_36 (42) = happyShift action_15
action_36 (44) = happyShift action_16
action_36 (45) = happyShift action_17
action_36 (4) = happyGoto action_51
action_36 (5) = happyGoto action_4
action_36 (6) = happyGoto action_5
action_36 (7) = happyGoto action_6
action_36 (10) = happyGoto action_7
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (15) = happyShift action_8
action_37 (16) = happyShift action_9
action_37 (17) = happyShift action_10
action_37 (18) = happyShift action_2
action_37 (20) = happyShift action_11
action_37 (30) = happyShift action_12
action_37 (34) = happyShift action_13
action_37 (35) = happyShift action_14
action_37 (42) = happyShift action_15
action_37 (44) = happyShift action_16
action_37 (45) = happyShift action_17
action_37 (4) = happyGoto action_50
action_37 (5) = happyGoto action_4
action_37 (6) = happyGoto action_5
action_37 (7) = happyGoto action_6
action_37 (10) = happyGoto action_7
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (15) = happyShift action_8
action_38 (16) = happyShift action_9
action_38 (17) = happyShift action_10
action_38 (18) = happyShift action_2
action_38 (20) = happyShift action_11
action_38 (30) = happyShift action_12
action_38 (34) = happyShift action_13
action_38 (35) = happyShift action_14
action_38 (42) = happyShift action_15
action_38 (44) = happyShift action_16
action_38 (45) = happyShift action_17
action_38 (4) = happyGoto action_49
action_38 (5) = happyGoto action_4
action_38 (6) = happyGoto action_5
action_38 (7) = happyGoto action_6
action_38 (10) = happyGoto action_7
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (15) = happyShift action_8
action_39 (16) = happyShift action_9
action_39 (17) = happyShift action_10
action_39 (18) = happyShift action_2
action_39 (20) = happyShift action_11
action_39 (30) = happyShift action_12
action_39 (34) = happyShift action_13
action_39 (35) = happyShift action_14
action_39 (42) = happyShift action_15
action_39 (44) = happyShift action_16
action_39 (45) = happyShift action_17
action_39 (4) = happyGoto action_48
action_39 (5) = happyGoto action_4
action_39 (6) = happyGoto action_5
action_39 (7) = happyGoto action_6
action_39 (10) = happyGoto action_7
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (15) = happyShift action_8
action_40 (16) = happyShift action_9
action_40 (17) = happyShift action_10
action_40 (18) = happyShift action_2
action_40 (20) = happyShift action_11
action_40 (30) = happyShift action_12
action_40 (34) = happyShift action_13
action_40 (35) = happyShift action_14
action_40 (42) = happyShift action_15
action_40 (44) = happyShift action_16
action_40 (45) = happyShift action_17
action_40 (4) = happyGoto action_47
action_40 (5) = happyGoto action_4
action_40 (6) = happyGoto action_5
action_40 (7) = happyGoto action_6
action_40 (10) = happyGoto action_7
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (15) = happyShift action_8
action_41 (16) = happyShift action_9
action_41 (17) = happyShift action_10
action_41 (18) = happyShift action_2
action_41 (20) = happyShift action_11
action_41 (30) = happyShift action_12
action_41 (34) = happyShift action_13
action_41 (35) = happyShift action_14
action_41 (42) = happyShift action_15
action_41 (44) = happyShift action_16
action_41 (45) = happyShift action_17
action_41 (4) = happyGoto action_46
action_41 (5) = happyGoto action_4
action_41 (6) = happyGoto action_5
action_41 (7) = happyGoto action_6
action_41 (10) = happyGoto action_7
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (15) = happyShift action_8
action_42 (16) = happyShift action_9
action_42 (17) = happyShift action_10
action_42 (18) = happyShift action_2
action_42 (20) = happyShift action_11
action_42 (30) = happyShift action_12
action_42 (34) = happyShift action_13
action_42 (35) = happyShift action_14
action_42 (42) = happyShift action_15
action_42 (44) = happyShift action_16
action_42 (45) = happyShift action_17
action_42 (4) = happyGoto action_45
action_42 (5) = happyGoto action_4
action_42 (6) = happyGoto action_5
action_42 (7) = happyGoto action_6
action_42 (10) = happyGoto action_7
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (28) = happyShift action_44
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (15) = happyShift action_8
action_44 (16) = happyShift action_9
action_44 (17) = happyShift action_10
action_44 (18) = happyShift action_2
action_44 (20) = happyShift action_11
action_44 (30) = happyShift action_12
action_44 (34) = happyShift action_13
action_44 (35) = happyShift action_14
action_44 (42) = happyShift action_15
action_44 (44) = happyShift action_16
action_44 (45) = happyShift action_17
action_44 (4) = happyGoto action_70
action_44 (5) = happyGoto action_4
action_44 (6) = happyGoto action_5
action_44 (7) = happyGoto action_6
action_44 (10) = happyGoto action_7
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (26) = happyShift action_33
action_45 (27) = happyShift action_34
action_45 (36) = happyShift action_35
action_45 (37) = happyShift action_36
action_45 (38) = happyShift action_37
action_45 (39) = happyShift action_38
action_45 (40) = happyShift action_39
action_45 (41) = happyShift action_40
action_45 (47) = happyShift action_41
action_45 (49) = happyShift action_42
action_45 _ = happyReduce_13

action_46 (26) = happyShift action_33
action_46 (27) = happyShift action_34
action_46 (36) = happyShift action_35
action_46 (37) = happyShift action_36
action_46 (38) = happyShift action_37
action_46 (39) = happyShift action_38
action_46 (40) = happyShift action_39
action_46 (41) = happyShift action_40
action_46 (47) = happyShift action_41
action_46 (49) = happyShift action_42
action_46 _ = happyReduce_12

action_47 (36) = happyFail []
action_47 (37) = happyFail []
action_47 (38) = happyFail []
action_47 (39) = happyFail []
action_47 (40) = happyFail []
action_47 (41) = happyFail []
action_47 _ = happyReduce_9

action_48 (36) = happyFail []
action_48 (37) = happyFail []
action_48 (38) = happyFail []
action_48 (39) = happyFail []
action_48 (40) = happyFail []
action_48 (41) = happyFail []
action_48 _ = happyReduce_8

action_49 (36) = happyFail []
action_49 (37) = happyFail []
action_49 (38) = happyFail []
action_49 (39) = happyFail []
action_49 (40) = happyFail []
action_49 (41) = happyFail []
action_49 _ = happyReduce_7

action_50 (36) = happyFail []
action_50 (37) = happyFail []
action_50 (38) = happyFail []
action_50 (39) = happyFail []
action_50 (40) = happyFail []
action_50 (41) = happyFail []
action_50 _ = happyReduce_6

action_51 (36) = happyFail []
action_51 (37) = happyFail []
action_51 (38) = happyFail []
action_51 (39) = happyFail []
action_51 (40) = happyFail []
action_51 (41) = happyFail []
action_51 _ = happyReduce_5

action_52 (36) = happyFail []
action_52 (37) = happyFail []
action_52 (38) = happyFail []
action_52 (39) = happyFail []
action_52 (40) = happyFail []
action_52 (41) = happyFail []
action_52 _ = happyReduce_4

action_53 (26) = happyFail []
action_53 (27) = happyFail []
action_53 (36) = happyShift action_35
action_53 (37) = happyShift action_36
action_53 (38) = happyShift action_37
action_53 (39) = happyShift action_38
action_53 (40) = happyShift action_39
action_53 (41) = happyShift action_40
action_53 _ = happyReduce_11

action_54 (26) = happyFail []
action_54 (27) = happyFail []
action_54 (36) = happyShift action_35
action_54 (37) = happyShift action_36
action_54 (38) = happyShift action_37
action_54 (39) = happyShift action_38
action_54 (40) = happyShift action_39
action_54 (41) = happyShift action_40
action_54 _ = happyReduce_10

action_55 _ = happyReduce_20

action_56 (33) = happyShift action_32
action_56 _ = happyReduce_19

action_57 (33) = happyShift action_32
action_57 _ = happyReduce_18

action_58 (31) = happyShift action_30
action_58 (32) = happyShift action_31
action_58 (33) = happyShift action_32
action_58 _ = happyReduce_17

action_59 (31) = happyShift action_30
action_59 (32) = happyShift action_31
action_59 (33) = happyShift action_32
action_59 _ = happyReduce_16

action_60 (15) = happyShift action_8
action_60 (16) = happyShift action_9
action_60 (17) = happyShift action_10
action_60 (18) = happyShift action_2
action_60 (20) = happyShift action_11
action_60 (30) = happyShift action_12
action_60 (34) = happyShift action_13
action_60 (35) = happyShift action_14
action_60 (42) = happyShift action_15
action_60 (44) = happyShift action_16
action_60 (45) = happyShift action_17
action_60 (4) = happyGoto action_69
action_60 (5) = happyGoto action_4
action_60 (6) = happyGoto action_5
action_60 (7) = happyGoto action_6
action_60 (10) = happyGoto action_7
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_23

action_62 _ = happyReduce_24

action_63 (24) = happyShift action_68
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_27

action_65 _ = happyReduce_40

action_66 (15) = happyShift action_8
action_66 (16) = happyShift action_9
action_66 (17) = happyShift action_10
action_66 (42) = happyShift action_20
action_66 (44) = happyShift action_16
action_66 (7) = happyGoto action_18
action_66 (11) = happyGoto action_67
action_66 _ = happyReduce_41

action_67 _ = happyReduce_43

action_68 (15) = happyShift action_8
action_68 (16) = happyShift action_9
action_68 (17) = happyShift action_10
action_68 (18) = happyShift action_2
action_68 (20) = happyShift action_11
action_68 (30) = happyShift action_12
action_68 (34) = happyShift action_13
action_68 (35) = happyShift action_14
action_68 (42) = happyShift action_15
action_68 (44) = happyShift action_16
action_68 (45) = happyShift action_17
action_68 (4) = happyGoto action_73
action_68 (5) = happyGoto action_4
action_68 (6) = happyGoto action_5
action_68 (7) = happyGoto action_6
action_68 (10) = happyGoto action_7
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (22) = happyShift action_72
action_69 (26) = happyShift action_33
action_69 (27) = happyShift action_34
action_69 (36) = happyShift action_35
action_69 (37) = happyShift action_36
action_69 (38) = happyShift action_37
action_69 (39) = happyShift action_38
action_69 (40) = happyShift action_39
action_69 (41) = happyShift action_40
action_69 (47) = happyShift action_41
action_69 (49) = happyShift action_42
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (19) = happyShift action_71
action_70 (26) = happyShift action_33
action_70 (27) = happyShift action_34
action_70 (36) = happyShift action_35
action_70 (37) = happyShift action_36
action_70 (38) = happyShift action_37
action_70 (39) = happyShift action_38
action_70 (40) = happyShift action_39
action_70 (41) = happyShift action_40
action_70 (47) = happyShift action_41
action_70 (49) = happyShift action_42
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (15) = happyShift action_8
action_71 (16) = happyShift action_9
action_71 (17) = happyShift action_10
action_71 (18) = happyShift action_2
action_71 (20) = happyShift action_11
action_71 (30) = happyShift action_12
action_71 (34) = happyShift action_13
action_71 (35) = happyShift action_14
action_71 (42) = happyShift action_15
action_71 (44) = happyShift action_16
action_71 (45) = happyShift action_17
action_71 (4) = happyGoto action_76
action_71 (5) = happyGoto action_4
action_71 (6) = happyGoto action_5
action_71 (7) = happyGoto action_6
action_71 (10) = happyGoto action_7
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (15) = happyShift action_8
action_72 (16) = happyShift action_9
action_72 (17) = happyShift action_10
action_72 (18) = happyShift action_2
action_72 (20) = happyShift action_11
action_72 (30) = happyShift action_12
action_72 (34) = happyShift action_13
action_72 (35) = happyShift action_14
action_72 (42) = happyShift action_15
action_72 (44) = happyShift action_16
action_72 (45) = happyShift action_17
action_72 (4) = happyGoto action_75
action_72 (5) = happyGoto action_4
action_72 (6) = happyGoto action_5
action_72 (7) = happyGoto action_6
action_72 (10) = happyGoto action_7
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (26) = happyShift action_33
action_73 (27) = happyShift action_34
action_73 (36) = happyShift action_35
action_73 (37) = happyShift action_36
action_73 (38) = happyShift action_37
action_73 (39) = happyShift action_38
action_73 (40) = happyShift action_39
action_73 (41) = happyShift action_40
action_73 (43) = happyShift action_74
action_73 (47) = happyShift action_41
action_73 (49) = happyShift action_42
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (25) = happyShift action_77
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (26) = happyShift action_33
action_75 (27) = happyShift action_34
action_75 (36) = happyShift action_35
action_75 (37) = happyShift action_36
action_75 (38) = happyShift action_37
action_75 (39) = happyShift action_38
action_75 (40) = happyShift action_39
action_75 (41) = happyShift action_40
action_75 (47) = happyShift action_41
action_75 (49) = happyShift action_42
action_75 _ = happyReduce_3

action_76 (26) = happyShift action_33
action_76 (27) = happyShift action_34
action_76 (36) = happyShift action_35
action_76 (37) = happyShift action_36
action_76 (38) = happyShift action_37
action_76 (39) = happyShift action_38
action_76 (40) = happyShift action_39
action_76 (41) = happyShift action_40
action_76 (47) = happyShift action_41
action_76 (49) = happyShift action_42
action_76 _ = happyReduce_1

action_77 (12) = happyShift action_80
action_77 (13) = happyShift action_81
action_77 (14) = happyShift action_82
action_77 (42) = happyShift action_83
action_77 (45) = happyShift action_84
action_77 (8) = happyGoto action_78
action_77 (9) = happyGoto action_79
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (24) = happyShift action_88
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_32

action_80 _ = happyReduce_38

action_81 _ = happyReduce_37

action_82 _ = happyReduce_39

action_83 (12) = happyShift action_80
action_83 (13) = happyShift action_81
action_83 (14) = happyShift action_82
action_83 (42) = happyShift action_83
action_83 (45) = happyShift action_84
action_83 (8) = happyGoto action_87
action_83 (9) = happyGoto action_79
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (12) = happyShift action_80
action_84 (13) = happyShift action_81
action_84 (14) = happyShift action_82
action_84 (42) = happyShift action_83
action_84 (45) = happyShift action_84
action_84 (46) = happyShift action_86
action_84 (8) = happyGoto action_85
action_84 (9) = happyGoto action_79
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (24) = happyShift action_90
action_85 (46) = happyShift action_92
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_36

action_87 (24) = happyShift action_90
action_87 (43) = happyShift action_91
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (12) = happyShift action_80
action_88 (13) = happyShift action_81
action_88 (14) = happyShift action_82
action_88 (42) = happyShift action_83
action_88 (45) = happyShift action_84
action_88 (8) = happyGoto action_89
action_88 (9) = happyGoto action_79
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (24) = happyShift action_90
action_89 _ = happyReduce_2

action_90 (12) = happyShift action_80
action_90 (13) = happyShift action_81
action_90 (14) = happyShift action_82
action_90 (42) = happyShift action_83
action_90 (45) = happyShift action_84
action_90 (8) = happyGoto action_93
action_90 (9) = happyGoto action_79
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_34

action_92 _ = happyReduce_35

action_93 (24) = happyShift action_90
action_93 _ = happyReduce_33

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
happyReduction_2 ((HappyAbsSyn8  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_8) `HappyStk`
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
happyReduction_14 (HappyAbsSyn10  happy_var_1)
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

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Times happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Divide happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Power happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  5 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  6 happyReduction_22
happyReduction_22 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (App happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Quot happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  6 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Rem happy_var_2 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  6 happyReduction_25
happyReduction_25 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Minus (Integer 0) happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  6 happyReduction_26
happyReduction_26 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  7 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  7 happyReduction_28
happyReduction_28 (HappyTerminal (INTVAL p happy_var_1))
	 =  HappyAbsSyn7
		 (Integer happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  7 happyReduction_29
happyReduction_29 (HappyTerminal (BOOLVAL p happy_var_1))
	 =  HappyAbsSyn7
		 (Boolean happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  7 happyReduction_30
happyReduction_30 (HappyTerminal (FLOATVAL p happy_var_1))
	 =  HappyAbsSyn7
		 (Float happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  7 happyReduction_31
happyReduction_31 (HappyTerminal (VAR p happy_var_1))
	 =  HappyAbsSyn7
		 (Variable happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  8 happyReduction_32
happyReduction_32 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  8 happyReduction_33
happyReduction_33 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  8 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  8 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (ListType happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  8 happyReduction_36
happyReduction_36 _
	_
	 =  HappyAbsSyn8
		 (EmptyList
	)

happyReduce_37 = happySpecReduce_1  9 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn9
		 (BoolType
	)

happyReduce_38 = happySpecReduce_1  9 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn9
		 (IntType
	)

happyReduce_39 = happySpecReduce_1  9 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn9
		 (FloatType
	)

happyReduce_40 = happySpecReduce_3  10 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (List happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  11 happyReduction_41
happyReduction_41  =  HappyAbsSyn11
		 ([]
	)

happyReduce_42 = happySpecReduce_1  11 happyReduction_42
happyReduction_42 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  11 happyReduction_43
happyReduction_43 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	INT p -> cont 12;
	BOOL p -> cont 13;
	FLOAT p -> cont 14;
	INTVAL p happy_dollar_dollar -> cont 15;
	FLOATVAL p happy_dollar_dollar -> cont 16;
	BOOLVAL p happy_dollar_dollar -> cont 17;
	LET p -> cont 18;
	IN p -> cont 19;
	IF p -> cont 20;
	THEN p -> cont 21;
	ELSE p -> cont 22;
	LAMBDA p -> cont 23;
	ARROW p -> cont 24;
	COLONS p -> cont 25;
	AND p -> cont 26;
	OR p -> cont 27;
	BIND p -> cont 28;
	PLUS p -> cont 29;
	MINUS p -> cont 30;
	TIMES p -> cont 31;
	DIVIDE p -> cont 32;
	POWER p -> cont 33;
	QUOT p -> cont 34;
	REM p -> cont 35;
	EQUALS p -> cont 36;
	GTEQ p -> cont 37;
	LTEQ p -> cont 38;
	NOTEQ p -> cont 39;
	GT' p -> cont 40;
	LT' p -> cont 41;
	LPAREN p -> cont 42;
	RPAREN p -> cont 43;
	VAR p happy_dollar_dollar -> cont 44;
	LBRACK p -> cont 45;
	RBRACK p -> cont 46;
	COLON p -> cont 47;
	COMMA p -> cont 48;
	PLUSPLUS p -> cont 49;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50 tk tks = happyError' (tks, explist)
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
   
  | Arrow TypeExp TypeExp
  
  | EmptyList
  | ListType TypeExp
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

  | Not
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
