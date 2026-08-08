module Lib
    ( mainFunc
    ) where

import Codec.Picture
import Codec.Picture.Metadata
import Ascii.ImageRenderer (renderImage)
import Ascii.ImageParser (getRawImageInfo, parseRawImageInfo, getProportionalHeight)
import Ascii.Downsampling.Algorithms.NearestNeighbour (downsampleNN)
import Ascii.Downsampling.Algorithms.MajorityVote (downsampleMV)
import Ascii.Downsampling.ImageConverter (juicyToMatrix)
import Ascii.Downsampling.DynamicImageChunker as DynamicImageChunker
import InputHelper

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[1;1H"

-- Main program flow
mainFunc :: IO ()
mainFunc = do
    clearScreen
    path <- prompt "Enter image path: "

    readingDynImgResult <- readImageWithMetadata path :: IO (Either String (DynamicImage, Metadatas))

    case readingDynImgResult of
       Left errorMessage ->
          putStrLn ("Failed to load image: " ++ errorMessage)

       Right (dynImg, metadata) -> do
          case parseRawImageInfo (getRawImageInfo metadata) of
              Left errorMessage ->
                  putStrLn errorMessage 

              Right (width, height) -> do
                  putStrLn ("Original Width: " ++ show width ++ " Original Height: " ++ show height)

                  algoChoice <- getAlgorithm
                  asciiRampChoice <- getAsciiRamp
                  isColoured <- getColourChoice 

                  let juicyImage = convertRGB8 dynImg :: Image PixelRGB8
                  let rawPixelMatrix = juicyToMatrix juicyImage

                  -- Execute branch based on selected algorithm
                  case algoChoice of
                      "1" -> do
                          targetSize  <- getOutputSize

                          let desiredHeight = getProportionalHeight width height targetSize
                          let listOfChunks = chunkPixelMatrix rawPixelMatrix targetSize desiredHeight

                          let downsampledImage = downsampleNN listOfChunks targetSize
                          let renderedHeight = desiredHeight

                          renderImage downsampledImage asciiRampChoice isColoured targetSize renderedHeight 0 0

                      "2" -> do
                          targetSize  <- getOutputSize

                          let desiredHeight = getProportionalHeight width height targetSize
                          let listOfChunks = chunkPixelMatrix rawPixelMatrix targetSize desiredHeight

                          let downsampledImage = downsampleMV listOfChunks targetSize
                          let renderedHeight = desiredHeight
                          renderImage downsampledImage asciiRampChoice isColoured targetSize renderedHeight 0 0

                      "3" -> do
                          putStrLn "Rendering original image without downsampling..."
                          renderImage rawPixelMatrix asciiRampChoice isColoured width height 0 0

                      _   -> putStrLn "Unexpected error occurred."
