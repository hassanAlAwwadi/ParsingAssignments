module CSharpCode where

import Prelude hiding ((<*), (*>), (<$), LT, GT, EQ)

import qualified Data.Map as M
import Data.Map ((!))
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Debug.Trace


data ValueOrAddress = Value | Address
    deriving Show

type Env = M.Map String Int

codeAlgebra :: CSharpAlgebra (Env -> Code) (Env -> Code) (Env -> Code) (Env -> ValueOrAddress -> Code)
codeAlgebra =
    (  fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatFor, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprFun)
    )

fClas :: Token -> [Env -> Code] -> Env -> Code
fClas _ ms env = [Bsr "main", HALT] ++ concatMap ($ env) ms

fMembDecl :: Decl -> Env -> Code
fMembDecl _ _ = []

fMembMeth :: Type -> Token -> [Decl] -> (Env -> Code) -> Env -> Code
fMembMeth t (LowerId x) ps s env = [LABEL x] ++ s newEnv ++ [RET] where
    varcount = length ps
    (newEnv,_) = foldl go (env, -varcount) ps
    go (e,n) (Decl _ (LowerId name)) = (M.insert name n e, n+1)

fStatDecl :: Decl -> Env -> Code
fStatDecl _ _ = []

fStatExpr :: (Env -> ValueOrAddress -> Code) -> Env -> Code
fStatExpr e env = e env Value ++ [pop]

fStatIf :: (Env -> ValueOrAddress -> Code) -> (Env -> Code) -> (Env -> Code) -> Env -> Code
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ s1 env ++ [BRA n2] ++ s2 env
    where
        c        = e env Value
        (n1, n2) = (codeSize $ s1 env, codeSize $ s2 env)

fStatWhile :: (Env -> ValueOrAddress -> Code) -> (Env -> Code) -> Env -> Code
fStatWhile e s1 env = [BRA n] ++ s1 env ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e env Value
        (n, k) = (codeSize $ s1 env, codeSize c)

fStatFor :: (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> (Env -> Code) -> Env -> Code
fStatFor e e2 e3 s1 env = c3 ++ [BRA n] ++ s1 env ++ c ++ c2 ++ [BRT (-(n + k + 2))]
    where
        c  = e3 env Value
        c2 = e2 env Value
        c3 = e  env Value
        (n, k) = (codeSize $ s1 env, codeSize c + codeSize c2)

fStatReturn :: (Env -> ValueOrAddress -> Code) -> Env -> Code
fStatReturn e env = e env Value ++ [STR R4, RET]

fStatBlock  :: [Env -> Code] -> Env -> Code
fStatBlock c e = concatMap ($ e) c

fExprCon :: Token -> a -> b -> Code
fExprCon (ConstInt n) _ _ = [LDC n]
fExprCon (ConstBool True) _ _ = [LDC (-1)]
fExprCon (ConstBool False) _ _ = [LDC 0]
fExprCon (ConstChar c) _ _ = [LDC (fromEnum c)]

fExprVar :: Token -> Env -> ValueOrAddress -> Code
fExprVar (LowerId x) env va = let loc = trace (show (x, M.toList env)) (env ! x) in  case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprFun :: Token -> [Env -> ValueOrAddress -> Code] -> Env -> ValueOrAddress -> Code
fExprFun (LowerId "print") vars env va = evald ++ [TRAP 0, AJS 1] where
    evald = vars >>= (($ va) . ($ env))
fExprFun (LowerId name) vars env va = evald ++ [LINK 0, Bsr name {-- might need Bsa?--}, UNLINK] ++ map (const pop) vars ++ [LDR R4] where
    evald = vars >>= (($ va) . ($ env)) 


fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> Env -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 env va = e2 env Value ++ [LDS 0]      ++ e1 env Address ++ [STA 0]
fExprOp (Operator "||")  e1 e2 env _ | any ( == formatInstr (LDC (-1))) (map formatInstr (e1 env Value))  =  [LDC (-1)]
                                  | otherwise =  e1 env Value ++ e2 env Value ++ [opCodes ! "||"] 
fExprOp (Operator "&&")  e1 e2 env _ | any ( == formatInstr (LDC 0)) (map formatInstr (e1 env Value)) =  [LDC 0]
                                  | otherwise =  e1 env Value ++ e2 env Value ++ [opCodes ! "&&"]                                  
fExprOp (Operator op)  e1 e2 env va = e1 env Value ++ e2 env Value ++ [opCodes ! op]



opCodes :: M.Map String Instr
opCodes = M.fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]


