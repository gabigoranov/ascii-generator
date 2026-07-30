module Lib
    ( mainFunc
    ) where

import Codec.Picture

mainFunc :: IO ()
mainFunc = do
    putStrLn "Enter image path:"
    path <- getLine

    putStrLn "Enter pixel at X:"
    pixelXPosInput <- getLine
    let pixelXPos = read pixelXPosInput :: Int

    putStrLn "Enter pixel at Y:"
    pixelYPosInput <- getLine
    let pixelYPos = read pixelYPosInput :: Int

    let image = readImage path
    
    putStrLn (show pixelXPos)
