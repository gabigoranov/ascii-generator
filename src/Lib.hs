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
import Ascii.Downsampling.ImageConverter (juicyToMatrix)
import Ascii.Downsampling.DynamicImageChunker as DynamicImageChunker
import qualified Ascii.Downsampling.ImageChunker as ImageChunker

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

-- Safely get the chunking strategy selection
getChunkingStrategy :: IO String
getChunkingStrategy = do
    putStrLn "Choose a chunking strategy:"
    putStrLn "1) Uniform Chunking"
    putStrLn "2) Dynamic Chunking"
    choice <- prompt "Enter choice (1 or 2): "
    if choice `elem` ["1", "2"]
        then return choice
        else do
            putStrLn "Error: Invalid choice."
            getChunkingStrategy

-- Safely get the algorithm selection (including the "No Downsampling" option)
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

-- Placeholder for Uniform Chunking logic
uniformChunking :: [[PixelRGB8]] -> Int -> [[[PixelRGB8]]]
uniformChunking matrix desiredSize = 
    let 
        (rows, cols) = (length matrix, length (matrix !! 0))

        chunkSize = ImageChunker.getChunkSize cols desiredSize 

        chunkedImage = ImageChunker.chunkPixelMatrix matrix rows chunkSize 0

    in ImageChunker.getListOfChunks chunkedImage desiredSize chunkSize 0 0

-- Scales the output height to preserve the image's aspect ratio
getProportionalHeight :: Int -> Int -> Int -> Int
getProportionalHeight width height desiredWidth =
    round (fromIntegral desiredWidth * fromIntegral height / fromIntegral width)

-- Dynamic Chunking logic using separate horizontal and vertical boundaries
dynamicChunking :: [[PixelRGB8]] -> Int -> Int -> [[[PixelRGB8]]]
dynamicChunking matrix desiredWidth desiredHeight = 
    let 
        (height, width) = (length matrix, length (matrix !! 0))

        horizontalChunkCoverage = DynamicImageChunker.getChunkCoverage width desiredWidth
        horizontalBoundaries = DynamicImageChunker.getBoundaries width horizontalChunkCoverage 0

        verticalGroupCoverage = DynamicImageChunker.getChunkCoverage height desiredHeight 
        verticalBoundaries = DynamicImageChunker.getBoundaries height verticalGroupCoverage 0

        chunkedByRowMatrix = DynamicImageChunker.chunkPixelMatrixHorizontally matrix horizontalBoundaries 0
    in DynamicImageChunker.getListOfChunks chunkedByRowMatrix horizontalBoundaries verticalBoundaries 0 0

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

                  -- 1. Ask for Downsampling Algorithm first
                  algoChoice <- getAlgorithm

                  let juicyImage = convertRGB8 dynImg :: Image PixelRGB8
                  let rawPixelMatrix = juicyToMatrix juicyImage

                  -- 2. Execute branch based on selected algorithm
                  case algoChoice of
                      "1" -> do
                          chunkChoice <- getChunkingStrategy
                          targetSize  <- getOutputSize

                          let desiredHeight = getProportionalHeight width height targetSize
                          let listOfChunks = case chunkChoice of
                                  "1" -> uniformChunking rawPixelMatrix targetSize
                                  _   -> dynamicChunking rawPixelMatrix targetSize desiredHeight

                          let downsampledImage = downsampleNN listOfChunks targetSize
                          let renderedHeight = case chunkChoice of
                                  "1" -> targetSize
                                  _   -> desiredHeight
                          renderImage downsampledImage targetSize renderedHeight 0 0

                      "2" -> do
                          chunkChoice <- getChunkingStrategy
                          targetSize  <- getOutputSize

                          let desiredHeight = getProportionalHeight width height targetSize
                          let listOfChunks = case chunkChoice of
                                  "1" -> uniformChunking rawPixelMatrix targetSize
                                  _   -> dynamicChunking rawPixelMatrix targetSize desiredHeight

                          let downsampledImage = downsampleMV listOfChunks targetSize
                          let renderedHeight = case chunkChoice of
                                  "1" -> targetSize
                                  _   -> desiredHeight
                          renderImage downsampledImage targetSize renderedHeight 0 0

                      "3" -> do
                          putStrLn "Rendering original image without downsampling..."
                          -- Pass raw [[PixelRGB8]] directly to the renderer without chunking
                          renderImage rawPixelMatrix width height 0 0

                      _   -> putStrLn "Unexpected error occurred."
