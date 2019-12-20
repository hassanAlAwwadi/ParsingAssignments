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
  ident         {TokenIdent }

%%
-- tussen de { } moet haskell code die sutff doet
Rule    : ident "->" Cmds   {}--{Rule ($1,$3)}
Cmds    :Cmds ',' Cmd       {}--{Command $1 $3}
Cmd     :go                 {}--{Go $1}
        | take              {}--{Take $1}
        | mark              {}--{Mark $1}
        | nothing           {}--{Nothing $1}
        | turn Dir          {}--{Turn $1 $2}
        | case Dir of Alts end {}--{Case $2 $4}
        | ident             {}--{Ident $1}
Dir     : left              {}--{Left $1}
        | right             {}--{Right $1}
        | front             {}--{Front $1}
Alts    : Alts ';' Alt      {}--{Alt $1 $3}
Alt     : Pat "->" Cmds     {}--{ Pattern $1 $3}
Pat     : empty             { Empty }
        | Lambda            { Lambda }
        | Debris            { Debris }
        | Asteroid          { Asteroid }
        | Boundary          { Boundary }
        | '_'               { Underscore }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
data Pat  =  Empty | Lambda | Debris | Asteroid | Boundary|Underscore
type Commands = [Command]
type Ident = String
type Heading = ()
type Program = [Rule]
type Rule = (Ident, Commands)
data Command = Go | Take | Mark | Nothing' | Turn
data Dir = Left' | Right' | Front
type Alts = [Alt]
type Alt = (Pat, Commands)

data Token = TokenArrow|TokenDot|TokenComma|TokenGo|TokenTake|TokenMark|TokenNothing
  |TokenTurn|TokenCase|TokenOf|TokenEnd
  |TokenLeft|TokenRight|TokenFront|TokenSemicolon
  |TokenEmpty|TokenLambda|TokenDebris|TokenAsteroid|TokenBoundary|TokenUnderscore
  |TokenIdent
	deriving (Eq,Show)
-- Ergens moet de alex lexer vandaan worden gehaald
-- lexer :: String -> [Token]
-- lexer = Lexer.alexScanTokens

-- main = getContents >>= print . calc . lexer
}

