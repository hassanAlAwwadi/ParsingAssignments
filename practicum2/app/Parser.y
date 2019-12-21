{
    module Parser where

import Lexer as L
import Types as T
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

Command     : go                    {T.Go}
            | take                  {T.Take}
            | mark                  {T.Mark}
            | nothing               {T.Nothing'}
            | turn Dir              {T.Turn $2}
            | case Dir of Alts end  {T.Case $2 $4}
            | Ident                 {T.CIdent $1}

Ident       : ident            {$1}

Dir         : left              {T.Left'}
            | right             {T.Right'}
            | front             {T.Front}

Alts        : Alts ';' Alt      {$3:$1}
            | Alt                  {[$1]}
Alt         : Pat "->" Commands     {($1, $3)}

Pat         :Contents           {T.Pat $1}            
            | '_'               {T.Underscore }
            
Contents    : empty             {T.Empty }
            | Lambda            {T.Lambda }
            | Debris            {T.Debris }
            | Asteroid          {T.Asteroid }
            | Boundary          {T.Boundary }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"



parseProgram :: String -> Program
parseProgram = calc . L.lexer
}

