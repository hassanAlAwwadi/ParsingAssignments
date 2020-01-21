module CSharpGram where

import Prelude hiding ((<*), (*>), (<$), ($>))
import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatFor    Expr Expr Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr0

pExpr0 :: Parser Token Expr
pExpr0 = chainr pExpr1 (ExprOper <$> sOperator0)

sOperator0 :: Parser Token Token
sOperator0 = satisfy isOperator
    where isOperator (Operator c) = case c of 
            "*" -> True
            "/" -> True
            "%" -> True
          isOperator _            = False

-- ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]

pExpr1 ::  Parser Token Expr
pExpr1 = chainr pExpr2 (ExprOper <$> sOperator1)

sOperator1 :: Parser Token Token
sOperator1 = satisfy isOperator
    where isOperator (Operator c) = case c of 
            "+" -> True
            "-" -> True
          isOperator _            = False

pExpr2 ::  Parser Token Expr
pExpr2 = chainl pExpr3 (ExprOper <$> sOperator2)

sOperator2 :: Parser Token Token
sOperator2 = satisfy isOperator
    where isOperator (Operator c) = case c of 
            ">"  -> True
            "<"  -> True 
            "<=" -> True 
            ">=" -> True 
          isOperator _            = False

pExpr3 ::  Parser Token Expr
pExpr3 = chainl pExpr4 (ExprOper <$> sOperator3)

sOperator3 :: Parser Token Token
sOperator3 = satisfy isOperator
    where isOperator (Operator c) = case c of 
            "==" -> True
            "!=" -> True
          isOperator _            = False

pExpr4 ::  Parser Token Expr
pExpr4 = chainl pExpr5 (ExprOper <$> sOperator4)

sOperator4 :: Parser Token Token
sOperator4 = satisfy isOperator
    where isOperator (Operator "^") = True
          isOperator _              = False

pExpr5 ::  Parser Token Expr
pExpr5 = chainl pExpr6 (ExprOper <$> sOperator5)

sOperator5 :: Parser Token Token
sOperator5 = satisfy isOperator
    where isOperator (Operator "&&") = True
          isOperator _               = False

pExpr6 ::  Parser Token Expr
pExpr6 = chainl pExpr7 (ExprOper <$> sOperator6)

sOperator6 :: Parser Token Token
sOperator6 = satisfy isOperator
    where isOperator (Operator "||") = True
          isOperator _               = False

pExpr7 ::  Parser Token Expr
pExpr7 = chainl pExprSimple (ExprOper <$> sOperator6)

sOperator7 :: Parser Token Token
sOperator7 = satisfy isOperator
    where isOperator (Operator "=") = True
          isOperator _              = False


pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr0 <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr0 <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr0 <*> pStat
     <|> StatFor    <$ symbol KeyFor    <*> parenthesised (pExpr0 <* sSemi *> pExpr0 <* sSemi *> pExpr0 <* sSemi ) *> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr0               <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])


pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)


pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

