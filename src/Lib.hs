module Lib
    ( mainFunc
    ) where

import Codec.Picture
import Codec.Picture.Metadata
import Ascii.ImageRenderer (renderImage)
import Ascii.ImageParser (getRawImageInfo, parseRawImageInfo)
import Ascii.Downsampling.NearestNeighbour (downsampleNN)
import Ascii.Downsampling.MajorityVote (downsampleMV)
import System.IO (hFlush, stdout)

-- Prompt helper that flushes stdout immediately so text appears before input
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- Safely get and validate the output pixel size limit
getOutputSize :: IO Int
getOutputSize = do
    input <- prompt "Enter output size (max 128 pixels): "
    case reads input :: [(Int, String)] of
        [(size, "")] -> 
            if size > 0 && size <= 128
                then return size
                else do
                    putStrLn "Error: Size must be between 1 and 128."
                    getOutputSize
        _ -> do
            putStrLn "Error: Please enter a valid integer."
            getOutputSize

-- Safely get the algorithm selection (including the new "No Downsampling" option)
getAlgorithm :: IO String
getAlgorithm = do
    putStrLn "Choose an operation mode:"
    putStrLn "1) Nearest Neighbour Downsampling (NN)"
    putStrLn "2) Majority Vote Downsampling (MV)"
    putStrLn "3) No Downsampling (Use Original Size)"
    choice <- prompt "Enter choice (1, 2, or 3): "
    if choice `elem` ["1", "2", "3"]
        then return choice
        else do
            putStrLn "Error: Invalid choice."
            getAlgorithm

-- Main program flow
mainFunc :: IO ()
mainFunc = do
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
                  let convertedImage = convertRGB8 dynImg :: Image PixelRGB8
          
                  -- Execute based on selected algorithm or skip entirely
                  case algoChoice of
                      "1" -> do
                          targetSize <- getOutputSize
                          downsampledImage <- downsampleNN convertedImage targetSize
                          renderImage downsampledImage targetSize targetSize 0 0

                      "2" -> do
                          targetSize <- getOutputSize
                          downsampledImage <- downsampleMV convertedImage targetSize
                          renderImage downsampledImage targetSize targetSize 0 0

                      "3" -> do
                          putStrLn "Rendering original image without downsampling..."
                          -- Pass the original width and height dynamically to your renderer
                          renderImage convertedImage width height 0 0

                      _   -> putStrLn "Unexpected error occurred."
