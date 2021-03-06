{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer as L
import Types as T
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (L.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,72) ([0,0,8,0,256,0,8192,0,0,0,4,0,0,0,0,0,0,1008,64,0,0,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3584,0,49152,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,32256,2048,0,0,0,32768,31,32768,8,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64512,4096,0,0,0,0,63,0,0,8192,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Program","Rules","Rule","Commands","Command","Ident","Dir","Alts","Alt","Pat","Contents","\"->\"","'.'","','","go","take","mark","nothing","turn","case","of","end","left","right","front","';'","empty","Lambda","Boundary","Asteroid","Debris","'_'","ident","%eof"]
        bit_start = st * 37
        bit_end = (st + 1) * 37
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..36]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (36) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (36) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (9) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (36) = happyShift action_5
action_2 (6) = happyGoto action_8
action_2 (9) = happyGoto action_4
action_2 _ = happyReduce_1

action_3 _ = happyReduce_2

action_4 (15) = happyShift action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_14

action_6 (37) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (18) = happyShift action_12
action_7 (19) = happyShift action_13
action_7 (20) = happyShift action_14
action_7 (21) = happyShift action_15
action_7 (22) = happyShift action_16
action_7 (23) = happyShift action_17
action_7 (36) = happyShift action_5
action_7 (7) = happyGoto action_9
action_7 (8) = happyGoto action_10
action_7 (9) = happyGoto action_11
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_3

action_9 (16) = happyShift action_23
action_9 (17) = happyShift action_24
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_6

action_11 _ = happyReduce_13

action_12 _ = happyReduce_7

action_13 _ = happyReduce_8

action_14 _ = happyReduce_9

action_15 _ = happyReduce_10

action_16 (26) = happyShift action_19
action_16 (27) = happyShift action_20
action_16 (28) = happyShift action_21
action_16 (10) = happyGoto action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (26) = happyShift action_19
action_17 (27) = happyShift action_20
action_17 (28) = happyShift action_21
action_17 (10) = happyGoto action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (24) = happyShift action_26
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_15

action_20 _ = happyReduce_16

action_21 _ = happyReduce_17

action_22 _ = happyReduce_11

action_23 _ = happyReduce_4

action_24 (18) = happyShift action_12
action_24 (19) = happyShift action_13
action_24 (20) = happyShift action_14
action_24 (21) = happyShift action_15
action_24 (22) = happyShift action_16
action_24 (23) = happyShift action_17
action_24 (36) = happyShift action_5
action_24 (8) = happyGoto action_25
action_24 (9) = happyGoto action_11
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_5

action_26 (30) = happyShift action_31
action_26 (31) = happyShift action_32
action_26 (32) = happyShift action_33
action_26 (33) = happyShift action_34
action_26 (34) = happyShift action_35
action_26 (35) = happyShift action_36
action_26 (11) = happyGoto action_27
action_26 (12) = happyGoto action_28
action_26 (13) = happyGoto action_29
action_26 (14) = happyGoto action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (25) = happyShift action_38
action_27 (29) = happyShift action_39
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_19

action_29 (15) = happyShift action_37
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_21

action_31 _ = happyReduce_23

action_32 _ = happyReduce_24

action_33 _ = happyReduce_27

action_34 _ = happyReduce_26

action_35 _ = happyReduce_25

action_36 _ = happyReduce_22

action_37 (18) = happyShift action_12
action_37 (19) = happyShift action_13
action_37 (20) = happyShift action_14
action_37 (21) = happyShift action_15
action_37 (22) = happyShift action_16
action_37 (23) = happyShift action_17
action_37 (36) = happyShift action_5
action_37 (7) = happyGoto action_41
action_37 (8) = happyGoto action_10
action_37 (9) = happyGoto action_11
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_12

action_39 (30) = happyShift action_31
action_39 (31) = happyShift action_32
action_39 (32) = happyShift action_33
action_39 (33) = happyShift action_34
action_39 (34) = happyShift action_35
action_39 (35) = happyShift action_36
action_39 (12) = happyGoto action_40
action_39 (13) = happyGoto action_29
action_39 (14) = happyGoto action_30
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_18

action_41 (17) = happyShift action_24
action_41 _ = happyReduce_20

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1++[happy_var_2]
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1++[happy_var_3]
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn8
		 (T.Go
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (T.Take
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (T.Mark
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn8
		 (T.Nothing'
	)

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (T.Turn happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 5 8 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (T.Case happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (T.CIdent happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyTerminal (L.TokenIdent happy_var_1))
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn10
		 (T.Left'
	)

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn10
		 (T.Right'
	)

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn10
		 (T.Front
	)

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1++[happy_var_3]
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (T.Pat happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn13
		 (T.Underscore
	)

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn14
		 (T.Empty
	)

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn14
		 (T.Lambda
	)

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn14
		 (T.Debris
	)

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn14
		 (T.Asteroid
	)

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn14
		 (T.Boundary
	)

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.TokenArrow -> cont 15;
	L.TokenDot -> cont 16;
	L.TokenComma -> cont 17;
	L.TokenGo -> cont 18;
	L.TokenTake -> cont 19;
	L.TokenMark -> cont 20;
	L.TokenNothing -> cont 21;
	L.TokenTurn -> cont 22;
	L.TokenCase -> cont 23;
	L.TokenOf -> cont 24;
	L.TokenEnd -> cont 25;
	L.TokenLeft -> cont 26;
	L.TokenRight -> cont 27;
	L.TokenFront -> cont 28;
	L.TokenSemicolon -> cont 29;
	L.TokenEmpty -> cont 30;
	L.TokenLambda -> cont 31;
	L.TokenBoundary -> cont 32;
	L.TokenAsteroid -> cont 33;
	L.TokenDebris -> cont 34;
	L.TokenUnderscore -> cont 35;
	L.TokenIdent happy_dollar_dollar -> cont 36;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 37 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(L.Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [L.Token] -> a
parseError _ = error "Parse error"



parseProgram :: String -> Program
parseProgram = calc . L.lexer
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc1900_0/ghc_2.h" #-}


























































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
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

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
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
