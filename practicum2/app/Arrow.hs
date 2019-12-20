module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Functor((<&>))
import Data.Maybe(mapMaybe, fromMaybe)
import Data.Map (Map, (!?))
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.List(permutations, intercalate, find)
import Data.Char (isSpace)


-- Beware: keys are (y,x) for some reason. 
type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq)

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

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


-- 2?
-- These three should be defined by you
type Commands = [Command]
type Ident = String
type Heading = (Int,Int)

type Program = [Rule]
type Rule = (Ident, Commands)
data Command = Go | Take | Mark | CNothing | Turn Dir | Case Dir Alts | CIdent Ident deriving (Eq)
data Dir = Left | Right | Front deriving (Eq)
type Alts = [Alt]
type Alt = (Pat, Commands)
data Pat = Pat Contents | Underscore deriving (Eq)

-- 4 WIP What can you find out from the Happy documentation over Happy’s handlingof left-recursive and right-recursive grammars.  How does this compare to the situationwhen using parser combinators?  Include your answer in a clearly marked comment.
-- Left recursion is more efficient in happy 
-- because right recursion wil overflow the
-- parse stack for long sequences of items. 

-- 5
type ProgramAlgebra r = [Rule] -> r
foldProgram :: ProgramAlgebra r -> Program -> r
foldProgram = ($)

type RuleAlgebra r = (Ident, Commands) -> r
foldRule :: RuleAlgebra r -> Rule -> r
foldRule = ($)

type CommandsAlgebra r = [Command] -> r
foldCommands :: CommandsAlgebra r -> Commands -> r
foldCommands = ($)

type CommandAlgebra r = (r, r, r, r, Dir -> r, Dir -> Alts -> r, Ident -> r)
foldCommand :: CommandAlgebra r -> Command -> r
foldCommand (go, take, mark, nothing, turn, fcase, cident) = f where 
    f Go   = go 
    f Take = take
    f Mark = mark 
    f CNothing = nothing 
    f (Turn d) = turn d
    f (Case d alts) = fcase d alts
    f (CIdent i) = cident i 

-- 6

checkAlgebra :: ProgramAlgebra Bool
checkAlgebra = combineAlgebra [rulesUsedDefinedAlgebra, startsAlgebra, rulesDefinedOnceAlgebra, noFailuresAlgebra]

check :: Program -> Bool
check = foldProgram checkAlgebra

combineAlgebra :: [ProgramAlgebra Bool] -> ProgramAlgebra Bool
combineAlgebra algs p = and $ foldProgram <$> algs <*> [p]

rulesUsedDefinedAlgebra :: ProgramAlgebra Bool
rulesUsedDefinedAlgebra p = and $ elem <$> rulesUsedAlgebra p <*> [rulesDefinedAlgebra p] 

rulesDefinedAlgebra :: ProgramAlgebra [Ident]
rulesDefinedAlgebra = map fst

rulesUsedAlgebra :: ProgramAlgebra [Ident]
rulesUsedAlgebra p = map snd p >>= rulesUsedCAlgebra

rulesUsedCAlgebra :: CommandsAlgebra [Ident]
rulesUsedCAlgebra = mapMaybe (foldCommand ruleUsedCAlgebra)

ruleUsedCAlgebra :: CommandAlgebra (Maybe Ident)
ruleUsedCAlgebra = (Nothing, Nothing, Nothing, Nothing, const Nothing, \_ -> const Nothing, Just)

startsAlgebra :: ProgramAlgebra Bool
startsAlgebra = any (\(n,_) -> n == "start")

rulesDefinedOnceAlgebra :: ProgramAlgebra Bool
rulesDefinedOnceAlgebra = singles . map fst where 
    singles [] = True
    singles (x:xs) = x `elem` xs && singles xs

noFailuresAlgebra  :: ProgramAlgebra Bool 
noFailuresAlgebra = all (foldCommands noFailuresCAlgebra) . map snd

noFailuresCAlgebra :: CommandsAlgebra Bool
noFailuresCAlgebra = all (foldCommand noFailureCAlgebra)

noFailureCAlgebra :: CommandAlgebra Bool
noFailureCAlgebra = (True, True, True, True, const True, const $ noFailure . map fst, const True) where
    noFailure :: [Pat] -> Bool
    noFailure pts =  Underscore `elem` pts || pts `elem` perms 
    perms = permutations [Pat Empty, Pat Lambda, Pat Debris, Pat Asteroid, Pat Boundary]



-- 7 
printSpace :: Space -> String
printSpace space = let 
    ((sizeY, sizeX),_) = L.findMax space 
    header = "(" ++ show sizeY ++ "," ++ show sizeX ++ ")\n" 
    in (header ++) . intercalate "\n" $ fmapf [0..sizeY] $ \y -> 
        fmapf [0..sizeX] $ \x -> 
            case space !? (y,x) of 
                Nothing       -> '.'
                Just Empty    -> '.' 
                Just Lambda   -> '\\' 
                Just Debris   -> '%'
                Just Asteroid -> 'O'
                Just Boundary -> '#'

fmapf = flip fmap

-- read and print file test 
printSpaceIO str = do
    lines <- lines . printSpace .  fst . head . parse parseSpace <$> readFile str
    mapM_ putStrLn lines

-- 8 

toEnvironment :: String -> Environment 
toEnvironment s = 
    let lexed  = undefined -- lexing  here 
        parsed = undefined -- parsing here 
    in  if check parsed then L.fromList parsed else L.empty


-- 9


-- data Command = Go | Take | Mark | CNothing | Turn Dir | Case Dir Alts | CIdent Ident deriving (Eq)
step :: Environment -> ArrowState -> Step
step env = step' where 
    step' (ArrowState s p h []    ) = Done s p h
    step' (ArrowState s p h (c:cs)) = case c of 
        Go          -> case s !? (p `vp` h) of 
            Just Asteroid -> Ok (ArrowState s p h cs)
            Just Boundary -> Ok (ArrowState s p h cs)
            _          -> Ok (ArrowState s (p `vp` h) h cs)
        Take       -> Ok (ArrowState (L.insert p Empty s) p h cs)
        Mark       -> Ok (ArrowState (L.insert p Lambda s) p h cs)
        CNothing   -> Ok (ArrowState s p h cs)
        Turn d -> Ok (ArrowState s p (turn d h) cs)
        Case d as  -> let col = Pat $ fromMaybe Empty $ s !? (p `vp` turn d h) 
                          instr = findInstr col as
                      in  case instr of 
                        Nothing -> Fail "No matching pattern" 
                        Just n  -> Ok (ArrowState s p h (n++cs))
        CIdent i   -> case env !? i of 
            Nothing -> Fail "Undefined function" 
            Just n  -> Ok (ArrowState s p h (n++cs))

vp :: (Int,Int) -> (Int, Int) -> (Int,Int)
vp (x,y) (v,w) = (x+v,y+w) 


turn  :: Dir -> Heading -> Heading 
turn Front h = h
turn Arrow.Left (0,1) = (-1,0)
turn Arrow.Left (-1,0) = (0,-1)
turn Arrow.Left (0,-1) = (1,0)
turn Arrow.Left (1,0) = (0,1)
turn Arrow.Right (0,1) = (1,0)
turn Arrow.Right (1,0) = (0,-1)
turn Arrow.Right (0,-1) = (-1,0)
turn Arrow.Right (-1,0) = (0,1)


findInstr :: Pat -> Alts -> Maybe Commands 
findInstr c alts = snd <$> case find (\a -> fst a ==c ) alts of 
    Nothing -> find (\a -> fst a == Underscore) alts 
    Just cs -> Just cs


-- 10 
-- blabla recursion. 

-- 11

interactive :: Environment -> ArrowState -> IO ()
interactive = undefined

batch :: Environment -> ArrowState -> (Space,Pos,Heading)
batch env state = case until done (batch' env) (Ok state) of 
    Done s p c -> (s,p,c)
    Fail s -> error s


batch' :: Environment -> Step -> Step
batch' env (Ok st) = step env st
batch' _ s          = s

done :: Step -> Bool
done (Ok st) = False
done _ = True