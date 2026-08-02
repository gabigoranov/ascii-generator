module Ascii.ImageRenderer (
    renderImage
) where

import Codec.Picture
import Ascii.PerceivedBrightness
import Ascii.CharRamp

renderImage :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> IO ()
renderImage _ _ _ (-1) _ = return ()
renderImage _ _ _ _ (-1) = return ()
renderImage image width height x y = do
    let pixel = pixelAt image x y :: PixelRGB8
                  
    let perceivedBrightness = calculatePerceivedBrightness pixel
    let normalizedBrightness = normalizePerceivedBrightness perceivedBrightness 
    let brightnessBracket = getBrightnessBracket normalizedBrightness 

    let asciiChar = asciiRamp !! brightnessBracket :: Char 
    
    let (nextX, nextY) = case (x, y) of
          (currX, currY) 
              | currX < (width - 1)  && currY < (height - 1)  -> (x + 1, y)
              | currX >= (width - 1) && currY < (height - 1)  -> (0, y + 1)
              | currX < (width - 1)  && currY >= (height - 1) -> (x + 1, y)
              | otherwise                               -> (-1, -1)

    if nextY > y
        then putStrLn [asciiChar]   
        else putStr [asciiChar, ' ']

    renderImage image width height nextX nextY 
