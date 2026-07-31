module Lib
    ( mainFunc
    ) where

import Codec.Picture

mainFunc :: IO ()
mainFunc = do
    putStrLn "Enter image path:"
    path <- getLine

    readingDynImgResult <- readImage path :: IO ( Either String DynamicImage )

    case readingDynImgResult of
       Left errorMessage ->
          putStrLn ("Failed to load image: " ++ errorMessage)
      
       Right dynImg -> do
          putStrLn ("Img loaded successfully!")
          
          let convertedImage = convertRGB8 dynImg :: Image PixelRGB8

          let testPixel = pixelAt convertedImage 256 256 :: PixelRGB8
          
          let (PixelRGB8 r g b) = testPixel

          putStrLn ("Loaded pixel: " ++ show r ++ ", " ++ show g ++ ", " ++ show b)
    

