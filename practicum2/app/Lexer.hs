{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  ->\s*         { \s -> Arrow }
  \.\s*         { \s -> Dot }
  ,\s*          { \s -> Comma }
  go\s*         { \s -> Go }
  take\s*       { \s -> Take }
  mark\s*       { \s -> Mark }
  nothing\s*    { \s -> Nothing }
  turn\s*       { \s -> Turn }
  case\s*       { \s -> Case }
  of\s*         { \s -> Of }
  end\s*        { \s -> End}
  left\s*       { \s -> Left}
  right\s*      { \s -> Right}
  front\s*      { \s -> Front}
  ;\s*  { \s -> Semicolon}
  empty\s*      { \s -> Empty}
  Lambda\s*     { \s -> Lambda}  
  Boundary\s*   { \s -> Boundary}
  --.*\n        { \s -> Debris} 

{
-- Each action has type :: String -> Token

-- The token type:
data Token = Arrow|Dot|Comma|Go|Take|Mark|Nothing
  |Turn|Case|Of|End
  |Left|Right|Front|Semicolon
  |Empty|Lambda|Debris|Asteroid|Boundary|Underscore
  |Idents
	deriving (Eq,Show)
data Idents = [Ident] 
data Ident = Letter|Digit|+|-
main = do
  s <- getContents
  print (alexScanTokens s)
}