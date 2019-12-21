{
    module Parser where

import Lexer
}
%name calc
%tokentype { Token }
%error { parseError }

%token
  "->"         {TokenArrow }
  '.'           {TokenDot }
  ','           {TokenComma }
  go            {TokenGo }
  take          {TokenTake }
  mark          {TokenMark }
  nothing       {TokenNothing }
  turn          {TokenTurn }
  case          {TokenCase }
  of            {TokenOf }
  end           {TokenEnd}
  left          {TokenLeft}
  right         {TokenRight}
  front         {TokenFront}
  ';'           {TokenSemicolon}
  empty         {TokenEmpty}
  Lambda        {TokenLambda}  
  Boundary      {TokenBoundary}
  Asteroid      {TokenAsteroid}
  Debris        {TokenDebris}
  '_'           {TokenUnderscore}
  ident         {TokenIdent $$}

%%

Program     : Rules    {$1}
            
Rules       : Rule                      {[$1]}
            | Rules Rule                {$2:$1}
Rule        : Ident "->" Commands '.'   {($1, $3)}

Commands    : Commands ',' Command       {$3:$1}
            | Command                   {[$1]}

Command     : go                    {Go}
            | take                  {Take}
            | mark                  {Mark}
            | nothing               {Nothing'}
            | turn Dir              {Turn $2}
            | case Dir of Alts end  {Case $2 $4}
            | Ident                 {CIdent $1}

Ident       : ident            {$1}

Dir         : left              {Left'}
            | right             {Right'}
            | front             {Front}

Alts        : Alts ';' Alt      {$3:$1}
            | Alt                  {[$1]}
Alt         : Pat "->" Commands     {($1, $3)}

Pat         :Contents           {Pat $1}            
            | '_'               { Underscore }
            
Contents    : empty             { Empty }
            | Lambda            { Lambda }
            | Debris            { Debris }
            | Asteroid          { Asteroid }
            | Boundary          { Boundary }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
type Program = [Rule] 
type Rule = (Ident, Commands)
type Ident = String
type Commands = [Command]


type Alts = [Alt]
type Alt = (Pat, Commands)
type Heading = (Int,Int)


data Command = Go | Take | Mark | Nothing' | Turn Dir | Case Dir Alts | CIdent Ident deriving (Eq)
data Dir = Left' | Right' | Front deriving (Eq)
data Pat = Pat Contents | Underscore deriving (Eq)

data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Show)

data Token = TokenArrow|TokenDot|TokenComma|TokenGo|TokenTake|TokenMark|TokenNothing
  |TokenTurn|TokenCase|TokenOf|TokenEnd
  |TokenLeft|TokenRight|TokenFront|TokenSemicolon
  |TokenEmpty|TokenLambda|TokenDebris|TokenAsteroid|TokenBoundary|TokenUnderscore
  |TokenIdent String
	deriving (Eq,Show)
-- Ergens moet de alex lexer vandaan worden gehaald
-- lexer :: String -> [Token]
-- lexer = Lexer.alexScanTokens

-- main = getContents >>= print . calc . lexer
}

