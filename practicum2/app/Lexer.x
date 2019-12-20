{
  module Lexer (lexer) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+				;
  \-\>          { \s -> Arrow }
  \.            { \s -> Dot }
  \,            { \s -> Comma }
  go            { \s -> Go }
  take          { \s -> Take }
  mark          { \s -> Mark }
  nothing       { \s -> Nothing' }
  turn          { \s -> Turn }
  case          { \s -> Case }
  of            { \s -> Of }
  end           { \s -> End}
  left          { \s -> Left'}
  right         { \s -> Right'}
  front         { \s -> Front}
  \;            { \s -> Semicolon}
  empty         { \s -> Empty}
  Lambda        { \s -> Lambda}  
  Boundary      { \s -> Boundary}
  \_            { \s -> Underscore}
  [$digit$alpha\+\-].* { \s -> Ident}
{
-- Each action has type :: String -> Token

-- The token type:
data Token = Arrow|Dot|Comma|Go|Take|Mark|Nothing'
  |Turn|Case|Of|End
  |Left'|Right'|Front|Semicolon
  |Empty|Lambda|Debris|Asteroid|Boundary|Underscore
  |Ident deriving (Eq,Show)

lexer = do
  s <- getContents
  print (alexScanTokens s)
}