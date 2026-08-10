module Ascii.ImageRenderer (
    generateImage,
    renderImage,
    exportImage
) where

import Codec.Picture hiding (generateImage)
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

-- Generates a single character as a string (colored if requested)
generateChar :: PixelRGB8 -> String -> Bool -> String
generateChar pixel asciiRamp isColoured =
    let brightnessBracket = ( ( getBrightnessBracket $ length asciiRamp ) . normalizeLuma . getLuma ) pixel
        asciiChar = asciiRamp !! brightnessBracket :: Char
        coloured = colorizeChar pixel asciiChar
    in if isColoured then coloured else [asciiChar]

-- Generates a row of character strings from a row of pixels
generateRow :: [PixelRGB8] -> String -> Bool -> [String]
generateRow pixelRow asciiRamp isColoured =
    [ generateChar pixel asciiRamp isColoured | pixel <- pixelRow ]

-- Generates a matrix of strings (one entry per character) representing the ASCII art
generateImage :: [[PixelRGB8]] -> String -> Bool -> [[String]]
generateImage image asciiRamp isColoured =
    [ generateRow row asciiRamp isColoured | row <- image ]

-- Converts the art matrix into a list of printable lines
formatArt :: [[String]] -> [String]
formatArt = map unwords

-- Renders a generated art matrix to the console
renderImage :: [[String]] -> IO ()
renderImage = mapM_ putStrLn . formatArt

-- Exports a generated art matrix to a txt file, exactly as it would print to the console
exportImage :: [[String]] -> FilePath -> IO ()
exportImage art outputPath =
    writeFile outputPath (unlines (formatArt art))
