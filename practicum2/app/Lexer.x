{
  module Lexer (lexer,Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$eol = [\n]
tokens :-
  $white+				;
  $eol            ;
  "->"       { \s -> TokenArrow }
  \.            { \s -> TokenDot }
  \,            { \s -> TokenComma }
  go            { \s -> TokenGo }
  take          { \s -> TokenTake }
  mark          { \s -> TokenMark }
  nothing       { \s -> TokenNothing }
  turn          { \s -> TokenTurn }
  case          { \s -> TokenCase }
  of            { \s -> TokenOf }
  end           { \s -> TokenEnd}
  left          { \s -> TokenLeft}
  right         { \s -> TokenRight}
  front         { \s -> TokenFront}
  \;             { \s -> TokenSemicolon}
  empty         { \s -> TokenEmpty}
  Lambda        { \s -> TokenLambda}  
  Boundary      { \s -> TokenBoundary}
  Asteroid      { \s -> TokenAsteroid}
  Debris        { \s -> TokenDebris}
  \_            { \s -> TokenUnderscore}
  $alpha [$alpha $digit]* { \s -> TokenIdent s}
{
-- Each action has type :: String -> Token

-- The token type:
data Token = TokenArrow|TokenDot|TokenComma|TokenGo|TokenTake|TokenMark|TokenNothing
  |TokenTurn|TokenCase|TokenOf|TokenEnd
  |TokenLeft|TokenRight|TokenFront|TokenSemicolon
  |TokenEmpty|TokenLambda|TokenDebris|TokenAsteroid|TokenBoundary|TokenUnderscore
  |TokenIdent String
	deriving (Eq,Show)

lexer = alexScanTokens
}