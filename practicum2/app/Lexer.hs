{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  ->\s*       { \s -> Arrow }
  \.\s*       { \s -> Dot }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = Arrow|Dot|Comma|Go|Take|Mark|Nothing
  |Turn|Case|Of|End
  |Left|Right|Front|Semicolon
  |Empty|Lambda|Debris|Asteroid|Boundary|Underscore
  |[Ident]
	deriving (Eq,Show)

data Ident = Letter|Digit|+|-
main = do
  s <- getContents
  print (alexScanTokens s)
}