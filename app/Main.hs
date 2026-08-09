module Main (main) where

import System.Console.CmdArgs
import ArgsConfig
import Lib

main :: IO ()
main = do 
    config <- cmdArgs configSpec 
    mainFunc config
