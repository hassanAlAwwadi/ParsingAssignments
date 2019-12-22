-- Made by Hassan Al-Awwadi (6998704) and Stephan van der Sman (6998445)

module Main where

import Lib
import Arrow
import Types
import Data.Map(findMax, (!?))
import Data.Functor(($>))
import Text.Read(readMaybe)
import Data.Char(toLower)
import System.Environment

-- 11

main :: IO ()
main = do
    print "Made by Hassan Al-Awwadi (6998704) and Stephan van der Sman (6998445)"

    print "Please provide the .arrow file"
    arrow <-  getLine >>= readEnvironment

    print "Please provide the .space file"
    space <- getLine >>= readSpace

    let (maxP,_) = findMax space
        minP = (0,0)
    print "Please provide the position"
    p <- getPos minP maxP

    print "Please provide the heading"
    h <- getHeading

    let stack = concat $ arrow !? "start"
        startState = ArrowState space p h stack

    print "Do you want to run in interactive mode"
    print "every single step is printed with full data, you can stop the process when you want."
    print "Press enter for Yes and anything else for No"
    g <- getLine
    case g of
        "" -> interactive arrow startState
        _  -> do
            print "Do you want to run in semi-interactive mode"
            print "Only When the ship changes position is the space reprinted"
            print "Press enter for Yes and anything else for No"
            g' <- getLine
            case g' of
                "" -> batchDebug arrow startState $> ()
                _  -> do
                    print "Running in batch mode. Only the final state will be returned"
                    let (s', p', h') = batch arrow startState
                    print "Final space:"
                    printSpaceIO s'

getPos :: (Int,Int) -> (Int, Int) -> IO (Int,Int)
getPos (minY,minX) (maxY,maxX) = do
    print $ "Please provide a tuple between " ++ show (minY,minX) ++ " and " ++ show (maxY,maxX)
    inp <- getLine
    case readMaybe inp of
        Nothing -> print "Sorry I didn't understand that. Please try again" *> getPos (minY,minX) (maxY,maxX)
        Just (y,x)  -> if (y >= minY && y <= maxY && x >= minX && x <= maxX)
            then return (y,x)
            else print "That input is out of bounds, please try again" *> getPos (minY,minX) (maxY,maxX)

getHeading :: IO (Int,Int)
getHeading  = do
    print "Please input d for down, u for up, l for left and r for right"
    inp <- map toLower <$> getLine
    case inp of
        "d" -> return (1,0)
        "u" -> return (-1,0)
        "l" -> return (0,-1)
        "r" -> return (0,1)
        _   -> print "Sorry I didn't understand that. Please try again" *> getHeading
