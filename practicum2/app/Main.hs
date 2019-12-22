-- By Hassan Al-Awwadi en Stephan (Stefan) van der Sman
module Main where

import Lib
import Arrow
import System.Environment

-- 11
main :: IO ()
main = do 
    print "By Hassan Al-Awwadi en Stephan (Stefan) van der Sman"
    args <- getArgs 
    case args of 
        (env:space:_) -> readInteractive env space
        _             -> print "please provide Arrow file as first argument and Space file as second argument"
