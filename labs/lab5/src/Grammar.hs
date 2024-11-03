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
happyExpList = Happy_Data_Array.listArray (0,353) ([49152,8203,3331,0,4,0,0,0,4096,0,12288,2016,5,0,15,0,56,40960,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24064,6400,104,7168,0,80,14336,0,160,28672,0,320,57344,36901,1665,0,0,0,32768,3,2560,0,0,0,1,0,32768,0,188,53298,0,49152,24448,20,0,0,1,224,32768,2,448,0,5,0,0,0,16384,61464,643,0,0,0,7168,12800,80,14336,25600,160,28672,51200,320,57344,36864,641,49152,8203,3331,32768,16407,6662,0,32815,13324,0,94,26649,0,188,53298,0,376,41060,1,752,16584,3,1504,33168,6,3008,800,13,6016,1600,26,0,32,0,24064,6400,104,0,0,2560,0,0,5120,0,384,10240,0,768,20480,0,1536,40960,0,3072,16384,1,6144,32768,2,12288,0,5,0,0,10,0,0,20,0,0,0,0,0,0,0,192,0,0,384,0,12032,3200,52,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,49152,1,1280,0,0,0,0,32815,13324,0,12544,2016,5,24640,4032,10,376,41060,1,752,16584,3,0,32259,81,0,1,0,0,63500,321,0,61464,643,448,0,8,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,28672,0,512,0,512,2048,49152,1,2048,0,2048,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseHasquelito","Expr","Form","Juxt","Atom","TypeExp","PrimType","List","ListMembers","Int","Bool","Float","int","float","bool","let","in","if","then","else","'\\\\'","\"->\"","\"::\"","\"&&\"","\"||\"","'='","'+'","'-'","'*'","'/'","quot","rem","\"==\"","\">=\"","\"<=\"","\"/=\"","'>'","'<'","'('","')'","var","'['","']'","':'","','","\"++\"","%eof"]
        bit_start = st * 49
        bit_end = (st + 1) * 49
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..48]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (15) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (17) = happyShift action_10
action_0 (18) = happyShift action_2
action_0 (20) = happyShift action_11
action_0 (30) = happyShift action_12
action_0 (33) = happyShift action_13
action_0 (34) = happyShift action_14
action_0 (41) = happyShift action_15
action_0 (43) = happyShift action_16
action_0 (44) = happyShift action_17
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (18) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (43) = happyShift action_42
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (26) = happyShift action_32
action_3 (27) = happyShift action_33
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

action_4 (29) = happyShift action_28
action_4 (30) = happyShift action_29
action_4 (31) = happyShift action_30
action_4 (32) = happyShift action_31
action_4 _ = happyReduce_15

action_5 (15) = happyShift action_8
action_5 (16) = happyShift action_9
action_5 (17) = happyShift action_10
action_5 (41) = happyShift action_20
action_5 (43) = happyShift action_16
action_5 (7) = happyGoto action_27
action_5 _ = happyReduce_20

action_6 _ = happyReduce_25

action_7 _ = happyReduce_14

action_8 _ = happyReduce_27

action_9 _ = happyReduce_29

action_10 _ = happyReduce_28

action_11 (15) = happyShift action_8
action_11 (16) = happyShift action_9
action_11 (17) = happyShift action_10
action_11 (18) = happyShift action_2
action_11 (20) = happyShift action_11
action_11 (30) = happyShift action_12
action_11 (33) = happyShift action_13
action_11 (34) = happyShift action_14
action_11 (41) = happyShift action_15
action_11 (43) = happyShift action_16
action_11 (44) = happyShift action_17
action_11 (4) = happyGoto action_26
action_11 (5) = happyGoto action_4
action_11 (6) = happyGoto action_5
action_11 (7) = happyGoto action_6
action_11 (10) = happyGoto action_7
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (15) = happyShift action_8
action_12 (16) = happyShift action_9
action_12 (17) = happyShift action_10
action_12 (41) = happyShift action_20
action_12 (43) = happyShift action_16
action_12 (7) = happyGoto action_25
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_8
action_13 (16) = happyShift action_9
action_13 (17) = happyShift action_10
action_13 (41) = happyShift action_20
action_13 (43) = happyShift action_16
action_13 (7) = happyGoto action_24
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (15) = happyShift action_8
action_14 (16) = happyShift action_9
action_14 (17) = happyShift action_10
action_14 (41) = happyShift action_20
action_14 (43) = happyShift action_16
action_14 (7) = happyGoto action_23
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_8
action_15 (16) = happyShift action_9
action_15 (17) = happyShift action_10
action_15 (18) = happyShift action_2
action_15 (20) = happyShift action_11
action_15 (23) = happyShift action_22
action_15 (30) = happyShift action_12
action_15 (33) = happyShift action_13
action_15 (34) = happyShift action_14
action_15 (41) = happyShift action_15
action_15 (43) = happyShift action_16
action_15 (44) = happyShift action_17
action_15 (4) = happyGoto action_21
action_15 (5) = happyGoto action_4
action_15 (6) = happyGoto action_5
action_15 (7) = happyGoto action_6
action_15 (10) = happyGoto action_7
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_30

action_17 (15) = happyShift action_8
action_17 (16) = happyShift action_9
action_17 (17) = happyShift action_10
action_17 (41) = happyShift action_20
action_17 (43) = happyShift action_16
action_17 (7) = happyGoto action_18
action_17 (11) = happyGoto action_19
action_17 _ = happyReduce_38

action_18 (47) = happyShift action_64
action_18 _ = happyReduce_39

action_19 (45) = happyShift action_63
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (15) = happyShift action_8
action_20 (16) = happyShift action_9
action_20 (17) = happyShift action_10
action_20 (18) = happyShift action_2
action_20 (20) = happyShift action_11
action_20 (30) = happyShift action_12
action_20 (33) = happyShift action_13
action_20 (34) = happyShift action_14
action_20 (41) = happyShift action_15
action_20 (43) = happyShift action_16
action_20 (44) = happyShift action_17
action_20 (4) = happyGoto action_21
action_20 (5) = happyGoto action_4
action_20 (6) = happyGoto action_5
action_20 (7) = happyGoto action_6
action_20 (10) = happyGoto action_7
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (26) = happyShift action_32
action_21 (27) = happyShift action_33
action_21 (35) = happyShift action_34
action_21 (36) = happyShift action_35
action_21 (37) = happyShift action_36
action_21 (38) = happyShift action_37
action_21 (39) = happyShift action_38
action_21 (40) = happyShift action_39
action_21 (42) = happyShift action_62
action_21 (46) = happyShift action_40
action_21 (48) = happyShift action_41
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (43) = happyShift action_61
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (15) = happyShift action_8
action_23 (16) = happyShift action_9
action_23 (17) = happyShift action_10
action_23 (41) = happyShift action_20
action_23 (43) = happyShift action_16
action_23 (7) = happyGoto action_60
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (15) = happyShift action_8
action_24 (16) = happyShift action_9
action_24 (17) = happyShift action_10
action_24 (41) = happyShift action_20
action_24 (43) = happyShift action_16
action_24 (7) = happyGoto action_59
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_24

action_26 (21) = happyShift action_58
action_26 (26) = happyShift action_32
action_26 (27) = happyShift action_33
action_26 (35) = happyShift action_34
action_26 (36) = happyShift action_35
action_26 (37) = happyShift action_36
action_26 (38) = happyShift action_37
action_26 (39) = happyShift action_38
action_26 (40) = happyShift action_39
action_26 (46) = happyShift action_40
action_26 (48) = happyShift action_41
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_21

action_28 (15) = happyShift action_8
action_28 (16) = happyShift action_9
action_28 (17) = happyShift action_10
action_28 (30) = happyShift action_12
action_28 (33) = happyShift action_13
action_28 (34) = happyShift action_14
action_28 (41) = happyShift action_20
action_28 (43) = happyShift action_16
action_28 (5) = happyGoto action_57
action_28 (6) = happyGoto action_5
action_28 (7) = happyGoto action_6
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (15) = happyShift action_8
action_29 (16) = happyShift action_9
action_29 (17) = happyShift action_10
action_29 (30) = happyShift action_12
action_29 (33) = happyShift action_13
action_29 (34) = happyShift action_14
action_29 (41) = happyShift action_20
action_29 (43) = happyShift action_16
action_29 (5) = happyGoto action_56
action_29 (6) = happyGoto action_5
action_29 (7) = happyGoto action_6
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (15) = happyShift action_8
action_30 (16) = happyShift action_9
action_30 (17) = happyShift action_10
action_30 (30) = happyShift action_12
action_30 (33) = happyShift action_13
action_30 (34) = happyShift action_14
action_30 (41) = happyShift action_20
action_30 (43) = happyShift action_16
action_30 (5) = happyGoto action_55
action_30 (6) = happyGoto action_5
action_30 (7) = happyGoto action_6
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (15) = happyShift action_8
action_31 (16) = happyShift action_9
action_31 (17) = happyShift action_10
action_31 (30) = happyShift action_12
action_31 (33) = happyShift action_13
action_31 (34) = happyShift action_14
action_31 (41) = happyShift action_20
action_31 (43) = happyShift action_16
action_31 (5) = happyGoto action_54
action_31 (6) = happyGoto action_5
action_31 (7) = happyGoto action_6
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (15) = happyShift action_8
action_32 (16) = happyShift action_9
action_32 (17) = happyShift action_10
action_32 (18) = happyShift action_2
action_32 (20) = happyShift action_11
action_32 (30) = happyShift action_12
action_32 (33) = happyShift action_13
action_32 (34) = happyShift action_14
action_32 (41) = happyShift action_15
action_32 (43) = happyShift action_16
action_32 (44) = happyShift action_17
action_32 (4) = happyGoto action_53
action_32 (5) = happyGoto action_4
action_32 (6) = happyGoto action_5
action_32 (7) = happyGoto action_6
action_32 (10) = happyGoto action_7
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (15) = happyShift action_8
action_33 (16) = happyShift action_9
action_33 (17) = happyShift action_10
action_33 (18) = happyShift action_2
action_33 (20) = happyShift action_11
action_33 (30) = happyShift action_12
action_33 (33) = happyShift action_13
action_33 (34) = happyShift action_14
action_33 (41) = happyShift action_15
action_33 (43) = happyShift action_16
action_33 (44) = happyShift action_17
action_33 (4) = happyGoto action_52
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
action_34 (33) = happyShift action_13
action_34 (34) = happyShift action_14
action_34 (41) = happyShift action_15
action_34 (43) = happyShift action_16
action_34 (44) = happyShift action_17
action_34 (4) = happyGoto action_51
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
action_35 (33) = happyShift action_13
action_35 (34) = happyShift action_14
action_35 (41) = happyShift action_15
action_35 (43) = happyShift action_16
action_35 (44) = happyShift action_17
action_35 (4) = happyGoto action_50
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
action_36 (33) = happyShift action_13
action_36 (34) = happyShift action_14
action_36 (41) = happyShift action_15
action_36 (43) = happyShift action_16
action_36 (44) = happyShift action_17
action_36 (4) = happyGoto action_49
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
action_37 (33) = happyShift action_13
action_37 (34) = happyShift action_14
action_37 (41) = happyShift action_15
action_37 (43) = happyShift action_16
action_37 (44) = happyShift action_17
action_37 (4) = happyGoto action_48
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
action_38 (33) = happyShift action_13
action_38 (34) = happyShift action_14
action_38 (41) = happyShift action_15
action_38 (43) = happyShift action_16
action_38 (44) = happyShift action_17
action_38 (4) = happyGoto action_47
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
action_39 (33) = happyShift action_13
action_39 (34) = happyShift action_14
action_39 (41) = happyShift action_15
action_39 (43) = happyShift action_16
action_39 (44) = happyShift action_17
action_39 (4) = happyGoto action_46
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
action_40 (33) = happyShift action_13
action_40 (34) = happyShift action_14
action_40 (41) = happyShift action_15
action_40 (43) = happyShift action_16
action_40 (44) = happyShift action_17
action_40 (4) = happyGoto action_45
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
action_41 (33) = happyShift action_13
action_41 (34) = happyShift action_14
action_41 (41) = happyShift action_15
action_41 (43) = happyShift action_16
action_41 (44) = happyShift action_17
action_41 (4) = happyGoto action_44
action_41 (5) = happyGoto action_4
action_41 (6) = happyGoto action_5
action_41 (7) = happyGoto action_6
action_41 (10) = happyGoto action_7
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (28) = happyShift action_43
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (15) = happyShift action_8
action_43 (16) = happyShift action_9
action_43 (17) = happyShift action_10
action_43 (18) = happyShift action_2
action_43 (20) = happyShift action_11
action_43 (30) = happyShift action_12
action_43 (33) = happyShift action_13
action_43 (34) = happyShift action_14
action_43 (41) = happyShift action_15
action_43 (43) = happyShift action_16
action_43 (44) = happyShift action_17
action_43 (4) = happyGoto action_68
action_43 (5) = happyGoto action_4
action_43 (6) = happyGoto action_5
action_43 (7) = happyGoto action_6
action_43 (10) = happyGoto action_7
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (46) = happyShift action_40
action_44 (48) = happyShift action_41
action_44 _ = happyReduce_13

action_45 (46) = happyShift action_40
action_45 (48) = happyShift action_41
action_45 _ = happyReduce_12

action_46 (26) = happyShift action_32
action_46 (27) = happyShift action_33
action_46 (35) = happyFail []
action_46 (36) = happyFail []
action_46 (37) = happyFail []
action_46 (38) = happyFail []
action_46 (39) = happyFail []
action_46 (40) = happyFail []
action_46 (46) = happyShift action_40
action_46 (48) = happyShift action_41
action_46 _ = happyReduce_9

action_47 (26) = happyShift action_32
action_47 (27) = happyShift action_33
action_47 (35) = happyFail []
action_47 (36) = happyFail []
action_47 (37) = happyFail []
action_47 (38) = happyFail []
action_47 (39) = happyFail []
action_47 (40) = happyFail []
action_47 (46) = happyShift action_40
action_47 (48) = happyShift action_41
action_47 _ = happyReduce_8

action_48 (26) = happyShift action_32
action_48 (27) = happyShift action_33
action_48 (35) = happyFail []
action_48 (36) = happyFail []
action_48 (37) = happyFail []
action_48 (38) = happyFail []
action_48 (39) = happyFail []
action_48 (40) = happyFail []
action_48 (46) = happyShift action_40
action_48 (48) = happyShift action_41
action_48 _ = happyReduce_7

action_49 (26) = happyShift action_32
action_49 (27) = happyShift action_33
action_49 (35) = happyFail []
action_49 (36) = happyFail []
action_49 (37) = happyFail []
action_49 (38) = happyFail []
action_49 (39) = happyFail []
action_49 (40) = happyFail []
action_49 (46) = happyShift action_40
action_49 (48) = happyShift action_41
action_49 _ = happyReduce_6

action_50 (26) = happyShift action_32
action_50 (27) = happyShift action_33
action_50 (35) = happyFail []
action_50 (36) = happyFail []
action_50 (37) = happyFail []
action_50 (38) = happyFail []
action_50 (39) = happyFail []
action_50 (40) = happyFail []
action_50 (46) = happyShift action_40
action_50 (48) = happyShift action_41
action_50 _ = happyReduce_5

action_51 (26) = happyShift action_32
action_51 (27) = happyShift action_33
action_51 (35) = happyFail []
action_51 (36) = happyFail []
action_51 (37) = happyFail []
action_51 (38) = happyFail []
action_51 (39) = happyFail []
action_51 (40) = happyFail []
action_51 (46) = happyShift action_40
action_51 (48) = happyShift action_41
action_51 _ = happyReduce_4

action_52 (26) = happyFail []
action_52 (27) = happyFail []
action_52 (46) = happyShift action_40
action_52 (48) = happyShift action_41
action_52 _ = happyReduce_11

action_53 (26) = happyFail []
action_53 (27) = happyFail []
action_53 (46) = happyShift action_40
action_53 (48) = happyShift action_41
action_53 _ = happyReduce_10

action_54 _ = happyReduce_19

action_55 _ = happyReduce_18

action_56 (31) = happyShift action_30
action_56 (32) = happyShift action_31
action_56 _ = happyReduce_17

action_57 (31) = happyShift action_30
action_57 (32) = happyShift action_31
action_57 _ = happyReduce_16

action_58 (15) = happyShift action_8
action_58 (16) = happyShift action_9
action_58 (17) = happyShift action_10
action_58 (18) = happyShift action_2
action_58 (20) = happyShift action_11
action_58 (30) = happyShift action_12
action_58 (33) = happyShift action_13
action_58 (34) = happyShift action_14
action_58 (41) = happyShift action_15
action_58 (43) = happyShift action_16
action_58 (44) = happyShift action_17
action_58 (4) = happyGoto action_67
action_58 (5) = happyGoto action_4
action_58 (6) = happyGoto action_5
action_58 (7) = happyGoto action_6
action_58 (10) = happyGoto action_7
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_22

action_60 _ = happyReduce_23

action_61 (24) = happyShift action_66
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_26

action_63 _ = happyReduce_37

action_64 (15) = happyShift action_8
action_64 (16) = happyShift action_9
action_64 (17) = happyShift action_10
action_64 (41) = happyShift action_20
action_64 (43) = happyShift action_16
action_64 (7) = happyGoto action_18
action_64 (11) = happyGoto action_65
action_64 _ = happyReduce_38

action_65 _ = happyReduce_40

action_66 (15) = happyShift action_8
action_66 (16) = happyShift action_9
action_66 (17) = happyShift action_10
action_66 (18) = happyShift action_2
action_66 (20) = happyShift action_11
action_66 (30) = happyShift action_12
action_66 (33) = happyShift action_13
action_66 (34) = happyShift action_14
action_66 (41) = happyShift action_15
action_66 (43) = happyShift action_16
action_66 (44) = happyShift action_17
action_66 (4) = happyGoto action_71
action_66 (5) = happyGoto action_4
action_66 (6) = happyGoto action_5
action_66 (7) = happyGoto action_6
action_66 (10) = happyGoto action_7
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (22) = happyShift action_70
action_67 (26) = happyShift action_32
action_67 (27) = happyShift action_33
action_67 (35) = happyShift action_34
action_67 (36) = happyShift action_35
action_67 (37) = happyShift action_36
action_67 (38) = happyShift action_37
action_67 (39) = happyShift action_38
action_67 (40) = happyShift action_39
action_67 (46) = happyShift action_40
action_67 (48) = happyShift action_41
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (19) = happyShift action_69
action_68 (26) = happyShift action_32
action_68 (27) = happyShift action_33
action_68 (35) = happyShift action_34
action_68 (36) = happyShift action_35
action_68 (37) = happyShift action_36
action_68 (38) = happyShift action_37
action_68 (39) = happyShift action_38
action_68 (40) = happyShift action_39
action_68 (46) = happyShift action_40
action_68 (48) = happyShift action_41
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (15) = happyShift action_8
action_69 (16) = happyShift action_9
action_69 (17) = happyShift action_10
action_69 (18) = happyShift action_2
action_69 (20) = happyShift action_11
action_69 (30) = happyShift action_12
action_69 (33) = happyShift action_13
action_69 (34) = happyShift action_14
action_69 (41) = happyShift action_15
action_69 (43) = happyShift action_16
action_69 (44) = happyShift action_17
action_69 (4) = happyGoto action_74
action_69 (5) = happyGoto action_4
action_69 (6) = happyGoto action_5
action_69 (7) = happyGoto action_6
action_69 (10) = happyGoto action_7
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (15) = happyShift action_8
action_70 (16) = happyShift action_9
action_70 (17) = happyShift action_10
action_70 (18) = happyShift action_2
action_70 (20) = happyShift action_11
action_70 (30) = happyShift action_12
action_70 (33) = happyShift action_13
action_70 (34) = happyShift action_14
action_70 (41) = happyShift action_15
action_70 (43) = happyShift action_16
action_70 (44) = happyShift action_17
action_70 (4) = happyGoto action_73
action_70 (5) = happyGoto action_4
action_70 (6) = happyGoto action_5
action_70 (7) = happyGoto action_6
action_70 (10) = happyGoto action_7
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (26) = happyShift action_32
action_71 (27) = happyShift action_33
action_71 (35) = happyShift action_34
action_71 (36) = happyShift action_35
action_71 (37) = happyShift action_36
action_71 (38) = happyShift action_37
action_71 (39) = happyShift action_38
action_71 (40) = happyShift action_39
action_71 (42) = happyShift action_72
action_71 (46) = happyShift action_40
action_71 (48) = happyShift action_41
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (25) = happyShift action_75
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (26) = happyShift action_32
action_73 (27) = happyShift action_33
action_73 (35) = happyShift action_34
action_73 (36) = happyShift action_35
action_73 (37) = happyShift action_36
action_73 (38) = happyShift action_37
action_73 (39) = happyShift action_38
action_73 (40) = happyShift action_39
action_73 (46) = happyShift action_40
action_73 (48) = happyShift action_41
action_73 _ = happyReduce_3

action_74 (26) = happyShift action_32
action_74 (27) = happyShift action_33
action_74 (35) = happyShift action_34
action_74 (36) = happyShift action_35
action_74 (37) = happyShift action_36
action_74 (38) = happyShift action_37
action_74 (39) = happyShift action_38
action_74 (40) = happyShift action_39
action_74 (46) = happyShift action_40
action_74 (48) = happyShift action_41
action_74 _ = happyReduce_1

action_75 (12) = happyShift action_78
action_75 (13) = happyShift action_79
action_75 (14) = happyShift action_80
action_75 (41) = happyShift action_81
action_75 (8) = happyGoto action_76
action_75 (9) = happyGoto action_77
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (24) = happyShift action_83
action_76 _ = happyReduce_2

action_77 _ = happyReduce_31

action_78 _ = happyReduce_35

action_79 _ = happyReduce_34

action_80 _ = happyReduce_36

action_81 (12) = happyShift action_78
action_81 (13) = happyShift action_79
action_81 (14) = happyShift action_80
action_81 (41) = happyShift action_81
action_81 (8) = happyGoto action_82
action_81 (9) = happyGoto action_77
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (24) = happyShift action_83
action_82 (42) = happyShift action_85
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (12) = happyShift action_78
action_83 (13) = happyShift action_79
action_83 (14) = happyShift action_80
action_83 (41) = happyShift action_81
action_83 (8) = happyGoto action_84
action_83 (9) = happyGoto action_77
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (24) = happyShift action_83
action_84 _ = happyReduce_32

action_85 _ = happyReduce_33

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (VAR p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (App (Bind happy_var_2 happy_var_6) happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 8 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn8  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (VAR p happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lambda happy_var_3 happy_var_5 happy_var_8
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
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
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
		 (NotEq happy_var_1 happy_var_3
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
		 (Add happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Div happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  5 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  6 happyReduction_21
happyReduction_21 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (App happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  6 happyReduction_22
happyReduction_22 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Quot happy_var_2 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Rem happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  6 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Negate happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  6 happyReduction_25
happyReduction_25 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  7 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  7 happyReduction_27
happyReduction_27 (HappyTerminal (INTVAL p happy_var_1))
	 =  HappyAbsSyn7
		 (Integer happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  7 happyReduction_28
happyReduction_28 (HappyTerminal (BOOLVAL p happy_var_1))
	 =  HappyAbsSyn7
		 (Boolean happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  7 happyReduction_29
happyReduction_29 (HappyTerminal (FLOATVAL p happy_var_1))
	 =  HappyAbsSyn7
		 (Float happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  7 happyReduction_30
happyReduction_30 (HappyTerminal (VAR p happy_var_1))
	 =  HappyAbsSyn7
		 (Variable happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  8 happyReduction_31
happyReduction_31 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  8 happyReduction_32
happyReduction_32 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  8 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  9 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn9
		 (BoolType
	)

happyReduce_35 = happySpecReduce_1  9 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn9
		 (IntType
	)

happyReduce_36 = happySpecReduce_1  9 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn9
		 (FloatType
	)

happyReduce_37 = happySpecReduce_3  10 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (List happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  11 happyReduction_38
happyReduction_38  =  HappyAbsSyn11
		 ([]
	)

happyReduce_39 = happySpecReduce_1  11 happyReduction_39
happyReduction_39 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  11 happyReduction_40
happyReduction_40 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 49 49 notHappyAtAll (HappyState action) sts stk []

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


data E a
  = Ok a
  | Failed String
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
  deriving (Eq,Show)

data AST
  = Integer Int
  | Float Float
  | Boolean Bool
  
  | LetIn String AST AST
  | IfThenElse AST AST AST

  | Lambda String AST TypeExp

  | App AST AST
  | Bind String AST

  | And AST AST
  | Or AST AST
  
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST

  | Quot AST AST
  | Rem AST AST

  | Equals AST AST
  | Lt AST AST
  | Gt AST AST

  | Negate AST
  | NotEq AST AST
  
  | Variable String

  | List [AST]
  | Cons AST AST
  | Concat AST AST

  deriving (Eq,Show)
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
