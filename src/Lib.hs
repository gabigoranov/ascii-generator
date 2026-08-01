module Lib
    ( mainFunc
    ) where

import Codec.Picture
import Codec.Picture.Metadata
import qualified Codec.Picture.Metadata as Metadata

asciiRamp :: String
asciiRamp = " .:-=+*#%@"

calculatePerceivedBrightness :: Int -> Int -> Int -> Float
calculatePerceivedBrightness r g b = ( fromIntegral r * 0.2126 ) + ( fromIntegral g * 0.7152 ) + ( fromIntegral b * 0.0722 ) 

normalizePerceivedBrightness :: Float -> Float
normalizePerceivedBrightness brightness = brightness / 255.0

getBrightnessBracket :: Float -> Int
getBrightnessBracket x
    | x == 1.00  = 9
    | otherwise = truncate (abs x * 10) `mod` 10

getImageInfo :: Metadata.Metadatas -> (Maybe Word, Maybe Word)
getImageInfo meta = 
    let width  = Metadata.lookup Metadata.Width meta
        height = Metadata.lookup Metadata.Height meta
    in (width, height)

extractImageInfo :: (Maybe Word, Maybe Word) -> ( Either String ( Int, Int)  )
extractImageInfo (rawWidth, rawHeight) = 
    case ( rawWidth, rawHeight ) of
        (Just w, Just h ) -> Right (fromIntegral w, fromIntegral h)
        (Just _, Nothing ) -> Left "Unknown image height!"
        (Nothing, Just _ ) -> Left "Unknown image width!"
        (Nothing, Nothing) -> Left "Unknown image dimensions!"

loopImage :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> IO ()
loopImage _ _ _ (-1) _ = return ()
loopImage _ _ _ _ (-1) = return ()
loopImage image width height x y = do
    let pixel = pixelAt image x y :: PixelRGB8
                  
    let (PixelRGB8 r g b) = pixel

    let perceivedBrightness = calculatePerceivedBrightness (fromIntegral r) (fromIntegral g) (fromIntegral b)
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
        else putStr [asciiChar]

    loopImage image width height nextX nextY 

mainFunc :: IO ()
mainFunc = do
    putStrLn "Enter image path:"
    path <- getLine

    readingDynImgResult <- readImageWithMetadata path :: IO ( Either String ( DynamicImage, Metadatas ) )

    case readingDynImgResult of
       Left errorMessage ->
          putStrLn ("Failed to load image: " ++ errorMessage)
      
       Right ( dynImg, metadata ) -> do
          case extractImageInfo ( getImageInfo ( metadata ) ) of
              Left errorMessage ->
                  putStrLn errorMessage 

              Right ( width, height ) -> do
                  putStrLn ("Width: " ++ show width ++ " Height: " ++ show height )
          
                  let convertedImage = convertRGB8 dynImg :: Image PixelRGB8
          
                  loopImage convertedImage width height 0 0    

