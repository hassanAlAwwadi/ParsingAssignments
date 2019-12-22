-- By Hassan Al-Awwadi en Stephan (Stefan) van der Sman

module Arrow where

import Prelude as P hiding ((<*), (<$))
import Types
import ParseLib.Abstract
import Data.Functor as F((<&>), ($>))
import Data.Maybe(mapMaybe, fromMaybe)
import Data.Map (Map, (!?))
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.List(permutations, intercalate, find)
import Data.Char (isSpace)
import Parser as PS


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
  choice (P.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]


-- 1
-- See Lexer.x and Lexer.hs

-- 2
-- In Types.hs

-- 3
-- see Parser.y and Paser.hs

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
    f Nothing' = nothing 
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
    singles (x:xs) = x `notElem` xs && singles xs

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
                Just Ship     -> 'X'

fmapf = flip fmap

-- read and print file test 
printSpaceIO :: Space -> IO () 
printSpaceIO spa = do
    let spalines =  lines $ printSpace spa
    mapM_ putStrLn spalines

readSpace :: String -> IO Space
readSpace str =   fst . head . parse parseSpace <$> readFile str

-- 8 

toEnvironment :: String -> Environment 
toEnvironment s = 
    let parsed = parseProgram s  
    in  if check parsed then L.fromList parsed else L.empty


-- for testing
readEnvironment :: String -> IO Environment 
readEnvironment s = toEnvironment . unlines . lines <$> readFile s 

-- 9

step :: Environment -> ArrowState -> Step
step env (ArrowState s p h [])     = Done s p h
step env (ArrowState s p h (c:cs)) = case c of 
    Go          -> case fromMaybe Boundary $ s !? (p `vp` h) of 
        Asteroid -> Ok (ArrowState s p h cs)
        Boundary -> Ok (ArrowState s p h cs)
        _          -> Ok (ArrowState s (p `vp` h) h cs)
    Take       -> Ok (ArrowState (L.insert p Empty s) p h cs)
    Mark       -> Ok (ArrowState (L.insert p Lambda s) p h cs)
    Nothing'   -> Ok (ArrowState s p h cs)
    Turn d -> Ok (ArrowState s p (turn d h) cs)
    Case d as  -> let con = Pat $ fromMaybe Boundary $ s !? (p `vp` turn d h) 
                      instr = findInstr con as
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
turn Left' (0,1) = (-1,0)
turn Left' (-1,0) = (0,-1)
turn Left' (0,-1) = (1,0)
turn Left' (1,0) = (0,1)
turn Right' (0,1) = (1,0)
turn Right' (1,0) = (0,-1)
turn Right' (0,-1) = (-1,0)
turn Right' (-1,0) = (0,1)


findInstr :: Pat -> Alts -> Maybe Commands 
findInstr c alts = snd <$> case find (\a -> fst a ==c ) alts of 
    Nothing -> find (\a -> fst a == Underscore) alts 
    Just cs -> Just cs


-- 10 
-- A recursive call at the end means that at most the size of the stack is looping on itself. 
-- A recursive call in the middle could lead to the stack blowing up in size.

-- 11

batch :: Environment -> ArrowState -> (Space,Pos,Heading)
batch env state = case until done (once env) (Ok state) of 
    Done s p c -> (s,p,c)
    Fail s -> error s


once :: Environment -> Step -> Step
once env (Ok st) = step env st
once _ s          = s

done :: Step -> Bool
done (Ok st) = False
done _ = True


interactive :: Environment -> ArrowState -> IO ()
interactive env as = do 
    next <- case as of 
        st@(ArrowState s p h stack) -> do
            print "current stack is: "
            print stack
            print "" 
            print "current space is: "
            printSpaceIO (L.insert p Ship s) 
            print  "" 
            print ("Position is: " ++ show p) 
            print ("Heading is: " ++ show h) 
            print  "please press enter to confirm"  
            print  "please press C to end simulation"   
            c <- getLine
            return $ case c of 
                "C" ->  Done s p h
                _   -> step env st
    case next of 
        Ok n -> interactive env n
        Done s' p' h' -> print (s',p',h')
        Fail s -> error s


untilM :: Monad m => (a -> Bool) -> (m a -> m a) -> m a -> m a
untilM q f ma = do 
    b <- q <$> ma 
    if b 
        then ma 
        else untilM q f (f ma)

batchFull :: String -> String -> IO (Space,Pos,Heading)
batchFull e s = do 
    env   <- readEnvironment e
    space <- readSpace s
    let stack = concat $ env !? "start"
        startState = ArrowState space (0,0) (1,1) stack
    return $ batch env startState



interactiveFull :: String -> String -> IO ()
interactiveFull e s = do 
    env   <- readEnvironment e
    space <- readSpace s
    let stack = concat $ env !? "start"
        startState = ArrowState space (0,0) (1,0) stack
    interactive env startState
