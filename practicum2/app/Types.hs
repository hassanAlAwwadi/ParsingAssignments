module Types where 
import Data.Map

-- Beware: keys are (y,x) for some reason.

type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)

-- Ship added for printing convenience 
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary | Ship deriving (Eq, Show)


type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

-- assignment 2 down here
type Program = [Rule] 
type Rule = (Ident, Commands)
type Ident = String
type Commands = [Command]


type Alts = [Alt]
type Alt = (Pat, Commands)
type Heading = (Int,Int)


data Command = Go | Take | Mark | Nothing' | Turn Dir | Case Dir Alts | CIdent Ident deriving (Eq,Show)
data Dir = Left' | Right' | Front deriving (Eq,Show)
data Pat = Pat Contents | Underscore deriving (Eq,Show)

