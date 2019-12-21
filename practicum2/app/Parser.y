{
    module Parser where

import Lexer as L
}
%name calc
%tokentype { L.Token }
%error { parseError }

%token
  "->"         {L.TokenArrow }
  '.'           {L.TokenDot }
  ','           {L.TokenComma }
  go            {L.TokenGo }
  take          {L.TokenTake }
  mark          {L.TokenMark }
  nothing       {L.TokenNothing }
  turn          {L.TokenTurn }
  case          {L.TokenCase }
  of            {L.TokenOf }
  end           {L.TokenEnd}
  left          {L.TokenLeft}
  right         {L.TokenRight}
  front         {L.TokenFront}
  ';'           {L.TokenSemicolon}
  empty         {L.TokenEmpty}
  Lambda        {L.TokenLambda}  
  Boundary      {L.TokenBoundary}
  Asteroid      {L.TokenAsteroid}
  Debris        {L.TokenDebris}
  '_'           {L.TokenUnderscore}
  ident         {L.TokenIdent $$}

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
parseError :: [L.Token] -> a
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


-- Ergens moet de alex lexer vandaan worden gehaald
--Todo


parseProgram :: String -> Program
parseProgram = calc . L.lexer
}

