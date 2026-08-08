module Ascii.ImageRenderer (
    renderImage
) where

import Codec.Picture
import Ascii.RelativeLuminance (normalizeLuma, getLuma, getBrightnessBracket)

-- Wraps an ASCII character in 24-bit terminal color codes
colorizeChar :: PixelRGB8 -> Char -> String
colorizeChar (PixelRGB8 r g b) ch =
    "\ESC[38;2;" 
    ++ show r ++ ";" 
    ++ show g ++ ";" 
    ++ show b ++ "m" 
    ++ [ch] 
    ++ "\ESC[0m"

-- TODO: Refactor
renderImage :: [[PixelRGB8]] -> String -> Bool -> Int -> Int -> Int -> Int -> IO ()
renderImage _ _ _ _ _ (-1) (-1) = return ()
renderImage image asciiRamp isColoured width height x y = do
    let pixel = ( image !! y ) !! x

    let brightnessBracket = ( ( getBrightnessBracket $ length asciiRamp ) . normalizeLuma . getLuma ) pixel

    let asciiChar = asciiRamp !! brightnessBracket :: Char 
 
    let (nextX, nextY) = case (x, y) of
          (currX, currY) 
              | currX < (width - 1)  && currY < (height - 1)  -> (x + 1, y)
              | currX >= (width - 1) && currY < (height - 1)  -> (0, y + 1)
              | currX < (width - 1)  && currY >= (height - 1) -> (x + 1, y)
              | otherwise                               -> (-1, -1)
        
    let coloured = colorizeChar pixel asciiChar 
    let output = if isColoured then coloured else [asciiChar]

    if nextY > y
        then putStrLn output
        else putStr (output ++ " ")

    renderImage image asciiRamp isColoured width height nextX nextY 
