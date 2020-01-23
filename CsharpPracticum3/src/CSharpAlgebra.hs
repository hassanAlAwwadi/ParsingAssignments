module CSharpAlgebra where

import CSharpLex
import CSharpGram


type CSharpAlgebra clas memb stat expr
    = (  
        Token -> [memb] -> clas -- deal with classes
        ,  ( Decl                             -> memb 
            , Type -> Token -> [Decl] -> stat  -> memb 
            )

        ,   ( Decl                  -> stat 
            , expr                  -> stat 
            , expr -> stat -> stat  -> stat  -- if statement
            , expr -> stat          -> stat
            , expr -> expr -> expr -> stat -> stat
            , expr                  -> stat
            , [stat]                -> stat
            )

        ,   ( Token                    -> expr -- constants
            , Token                    -> expr -- vars
            , Token -> expr  -> expr   -> expr -- operators
            , Token -> [expr] -> expr -- functions
            )
    )


foldCSharp :: CSharpAlgebra clas memb stat expr -> Class -> clas
foldCSharp (c1, (m1,m2), (s1,s2,s3,s4,sf,s5,s6), (ec,ev,eo, ef)) = fClas
    where
        fClas (Class      c ms)       = c1 c (map fMemb ms)
        fMemb (MemberD    d)          = m1 d
        fMemb (MemberM    t m ps s)   = m2 t m ps (fStat s)
        fStat (StatDecl   d)          = s1 d
        fStat (StatExpr   e)          = s2 (fExpr e)
        fStat (StatIf     e l r)      = s3 (fExpr e) (fStat l) (fStat r)
        fStat (StatWhile  e d)        = s4 (fExpr e) (fStat d)
        fStat (StatFor    e e' e'' s) = sf (fExpr e) (fExpr e') (fExpr e'') (fStat s)
        fStat (StatReturn e)          = s5 (fExpr e)
        fStat (StatBlock  ss)         = s6 (map fStat ss)
        fExpr (ExprConst  con)        = ec con
        fExpr (ExprVar    var)        = ev var
        fExpr (ExprOper   op l r)     = eo op  (fExpr l) (fExpr r)
        fExpr (ExprFun    n args)     = ef n (fExpr <$> args)