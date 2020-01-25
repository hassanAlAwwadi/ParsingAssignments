module CSharpCode where

import Prelude hiding ((<*), (*>), (<$), LT, GT, EQ)

import qualified Data.Map as M
import Data.Function
import Data.Map ((!))
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Debug.Trace


data ValueOrAddress = Value | Address
    deriving Show

type Env = M.Map String Int

-- just a function to clean up generated code somewhat
clean :: Code -> Code 
clean (RET:_) = [RET]
clean [] = []
clean (AJS n:AJS m:r) 
    | n + m == 0 = clean r
    | otherwise  = clean (AJS (n+m) : r)
clean (n:m) = n : clean m


codeAlgebra :: CSharpAlgebra (Env -> Code) (Env -> Code) (Env -> (Code,Env)) (Env -> ValueOrAddress -> Code)
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

fMembMeth :: Type -> Token -> [Decl] -> (Env -> (Code, Env)) -> Env -> Code
fMembMeth t (LowerId x) ps s env = [LABEL x, LINK stmcount] ++ statements where
    varcount = length ps
    -- the arguments are stored before the markpointer. first argument is at -2, because -1 is used for the return address.
    (newEnv,_) = foldl go (env, (-varcount)-1) ps 
    go (e,n) (Decl _ (LowerId name)) = (M.insert name n e, n+1)
    (statements,_) = (\(a,b) -> (clean (a ++ [UNLINK, RET]) ,b)) $ s newEnv
    -- every line of code is a potential declaration of a var. 
    -- except for the UNLINK and RET at the end
    stmcount = length statements - 2 

fStatDecl :: Decl -> Env -> (Code, Env)
fStatDecl (Decl _ (LowerId name)) env = ([], M.insert name (maximum (0 : M.elems env) +1) env)

fStatExpr :: (Env -> ValueOrAddress -> Code) -> Env -> (Code, Env)
fStatExpr e env = (e env Value ++ [pop],env)

fStatIf :: (Env -> ValueOrAddress -> Code)
        -> (Env -> (Code, Env))
        -> (Env -> (Code, Env))
        -> Env
        -> (Code, Env)
fStatIf e s1 s2 env = (co ++ [BRF (n1 + 2)] ++ th ++ [BRA n2] ++ el, env)
    where
        co = e env Value
        (th, _) = s1 env -- no matter what is declared in the then/else part, it isn't allowed to escape its scope
        (el, _) = s2 env -- so we throw away the updated environment
        n1          = codeSize th
        n2          = codeSize el

fStatWhile  :: (Env -> ValueOrAddress -> Code)
            -> (Env -> (Code, Env)) -> Env -> (Code, Env)
fStatWhile e s env = ([BRA n] ++ d ++ c ++ [BRT (-(n + k + 2))],env)
    where
        c = e env Value
        k = codeSize c
        (d,_) = s env 
        n = codeSize d

fStatFor    :: (Env -> ValueOrAddress -> Code)
            -> (Env -> ValueOrAddress -> Code)
            -> (Env -> ValueOrAddress -> Code)
            -> (Env -> (Code, Env))
            -> Env
            -> (Code, Env)
fStatFor e e2 e3 s env = (con ++ [BRA n] ++ d ++ c ++ c2 ++ [BRT (-(n + k + 2))], env)
    where
        c  = e3 env Value
        c2 = e2 env Value
        con = e  env Value
        (d,_) = s env
        (n, k) = (codeSize d, codeSize c + codeSize c2)

fStatReturn :: (Env -> ValueOrAddress -> Code)-> Env -> (Code, Env)
fStatReturn e env = (e env Value ++ [STR R4, UNLINK, RET], env)

fStatBlock  :: [Env -> (Code, Env)] -> Env -> (Code, Env)
fStatBlock codegens env = (finalcode,finalenv) where 
    (finalcode,finalenv) = go codegens env []
    go []    old acc = (acc,old)
    go (f:r) old acc = let (code, new) = f old in go r new (acc ++ code)

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
fExprFun (LowerId name) vars env va = evald ++ Bsr name : map (const pop) vars ++ [LDR R4] where
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


