module Ascii.RelativeLuminance(
    getLuma,
    normalizeLuma,
    getBrightnessBracket
) where
import Codec.Picture

redLuma :: Float
redLuma = 0.2126

greenLuma :: Float
greenLuma = 0.7152

blueLuma :: Float
blueLuma = 0.0722

-- Calculates the approximate brightness level on a scale simillar to our vision
getLuma :: PixelRGB8 -> Float
getLuma pixel =
    let (PixelRGB8 r g b) = pixel
    in 
        ( fromIntegral r * redLuma ) + 
        ( fromIntegral g * greenLuma ) +
        ( fromIntegral b * blueLuma ) 

-- Normalizes the perceived brightness on a scale from 0 - 1
normalizeLuma :: Float -> Float
normalizeLuma brightness = brightness / 255.0

-- Classify a brightness in a bracket from 0 - 9 to map to 10 possible brightness ascii
getBrightnessBracket :: Int -> Float -> Int
getBrightnessBracket totalBracketsCount x
    | x >= 1.0   = totalBracketsCount - 1
    | x <= 0.0 = 0
    | otherwise = floor (x * fromIntegral totalBracketsCount) 

