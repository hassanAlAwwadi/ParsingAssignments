module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)


type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

--2 WIP
data Program = Functions  [(Name,FunctionCode)]
data FunctionCode = Case String Cases | Commands
type Cases = [(String,Commands)]
type Name = String

--4 WIP What can you find out from the Happy documentation over Happy’s handlingof left-recursive and right-recursive grammars.  How does this compare to the situationwhen using parser combinators?  Include your answer in a clearly marked comment.
-- Left recursion is more efficient in happy 
-- because right recursion wil overflow the
-- parse stack for long sequences of items. 


-- These three should be defined by you
type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

-- 7 big nono
-- printSpace :: Space -> Int -> String
-- printSpace space size= "(" ++ size ++ "," ++ size ++ ")" ++ printSpace' size size
--   where content x y = space L.! (x,y)
--         printSpace' 0 0 = getItem (contentsTable (content 0 0)):[]
--         printSpace' x 0 = (printSpace' x size):"\n" :getItem (contentsTable (content 0 0))
--         printSpace' x y = (printSpace' x (y-1)) :getItem (contentsTable (content x y))
--         getItem (contents, char) = char
        