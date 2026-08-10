{-# LANGUAGE RecordWildCards #-}
module Lib
    ( mainFunc
    ) where

import Codec.Picture hiding (generateImage)
import Codec.Picture.Metadata
import Ascii.ImageRenderer (generateImage, renderImage, exportImage)
import Ascii.ImageParser (getRawImageInfo, parseRawImageInfo, getProportionalHeight)
import Ascii.Downsampling.Algorithms.DownsamplingAlgorithm (DownsamplingAlgorithm(..))
import Ascii.Downsampling.Algorithms.NearestNeighbour (downsampleNN)
import Ascii.Downsampling.Algorithms.MajorityVote (downsampleMV)
import Ascii.Downsampling.ImageConverter (juicyToMatrix)
import Ascii.Downsampling.DynamicImageChunker as DynamicImageChunker
import ArgsConfig (Config(..))
import qualified Ascii.CharRamp as AsciiRamp
import Ascii.CharRamp

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[1;1H"

-- Main program flow
mainFunc :: Config -> IO ()
mainFunc Config{..} = do
    clearScreen

    readingDynImgResult <- readImageWithMetadata imgPath :: IO (Either String (DynamicImage, Metadatas))

    case readingDynImgResult of
       Left errorMessage ->
          putStrLn ("Failed to load image: " ++ errorMessage)

       Right (dynImg, metadata) -> do
          case parseRawImageInfo (getRawImageInfo metadata) of
              Left errorMessage ->
                  putStrLn errorMessage 

              Right (width, height) -> do
                  putStrLn ("Original Width: " ++ show width ++ " Original Height: " ++ show height)

                  let asciiRampChoice = if levelOfDetail == AsciiRamp.Standart then asciiRamp else detailedAsciiRamp 

                  let juicyImage = convertRGB8 dynImg :: Image PixelRGB8
                  let rawPixelMatrix = juicyToMatrix juicyImage

                  -- Execute branch based on selected algorithm
                  case algorithmChoice of
                      NearestNeighbour -> do
                          let desiredHeight = getProportionalHeight width height outputWidth
                          let listOfChunks = chunkPixelMatrix rawPixelMatrix outputWidth desiredHeight

                          let downsampledImage = downsampleNN listOfChunks outputWidth

                          handleRenderedImage asciiRampChoice downsampledImage

                      MajorityVote -> do
                          let desiredHeight = getProportionalHeight width height outputWidth
                          let listOfChunks = chunkPixelMatrix rawPixelMatrix outputWidth desiredHeight

                          let downsampledImage = downsampleMV listOfChunks outputWidth

                          handleRenderedImage asciiRampChoice downsampledImage
  where
      -- Render the art to the console, or export it to a file if an output path is given
      handleRenderedImage :: String -> [[PixelRGB8]] -> IO ()
      handleRenderedImage asciiRampChoice downsampledImage = do
          let art = generateImage downsampledImage asciiRampChoice isColoured

          case outputPath of
              Nothing          -> renderImage art
              Just outFilePath -> exportImage art outFilePath
