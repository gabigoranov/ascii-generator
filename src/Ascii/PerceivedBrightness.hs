module Ascii.PerceivedBrightness (
    calculatePerceivedBrightness,
    normalizePerceivedBrightness,
    getBrightnessBracket
) where
import Codec.Picture

redBrightnessModifier :: Float
redBrightnessModifier = 0.2126

greenBrightnessModifier :: Float
greenBrightnessModifier = 0.7152

blueBrightnessModifier :: Float
blueBrightnessModifier = 0.0722

-- Calculates the approximate brightness level on a scale simillar to our vision
calculatePerceivedBrightness :: PixelRGB8 -> Float
calculatePerceivedBrightness pixel =
    let (PixelRGB8 r g b) = pixel
    in 
        ( fromIntegral r * redBrightnessModifier ) + 
        ( fromIntegral g * greenBrightnessModifier ) +
        ( fromIntegral b * blueBrightnessModifier ) 

-- Normalizes the perceived brightness on a scale from 0 - 1
normalizePerceivedBrightness :: Float -> Float
normalizePerceivedBrightness brightness = brightness / 255.0

-- Classify a brightness in a bracket from 0 - 9 to map to 10 possible brightness ascii
getBrightnessBracket :: Float -> Int
getBrightnessBracket x
    | x == 1.00  = 9
    | otherwise = truncate (abs x * 10) `mod` 10
