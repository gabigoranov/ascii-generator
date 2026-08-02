module Lib
    ( mainFunc
    ) where

import Codec.Picture
import Codec.Picture.Metadata
import Ascii.ImageRenderer (renderImage)
import Ascii.ImageParser (getRawImageInfo, parseRawImageInfo)
import Ascii.Downsampling.NearestNeighbour (downsampleNN)

-- Main program flow
mainFunc :: IO ()
mainFunc = do
    putStrLn "Enter image path:"
    path <- getLine

    readingDynImgResult <- readImageWithMetadata path :: IO ( Either String ( DynamicImage, Metadatas ) )

    case readingDynImgResult of
       Left errorMessage ->
          putStrLn ("Failed to load image: " ++ errorMessage)
      
       Right ( dynImg, metadata ) -> do
          case parseRawImageInfo ( getRawImageInfo ( metadata ) ) of
              Left errorMessage ->
                  putStrLn errorMessage 

              Right ( width, height ) -> do
                  putStrLn ("Width: " ++ show width ++ " Height: " ++ show height )
          
                  let convertedImage = convertRGB8 dynImg :: Image PixelRGB8
                  downsampledImage <- downsampleNN convertedImage 16

                  renderImage downsampledImage 16 16 0 0    

