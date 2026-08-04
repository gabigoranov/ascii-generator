module Ascii.Downsampling.NearestNeighbour (
    downsampleNN
) where

import Codec.Picture
import Ascii.Downsampling.ImageChunker
import Ascii.Downsampling.ImageConverter

-- Extract the top left most pixel in a grid
getNNDownsampledPixel :: [[PixelRGB8]] -> PixelRGB8
getNNDownsampledPixel pixelMatrix = (pixelMatrix !! 0) !! 0

-- Controls the flow of downsampling with NN
downsampleNN :: [[PixelRGB8]] -> Int -> [[PixelRGB8]]
downsampleNN pixelMatrix desiredSize = 
    let (height, width) = (length pixelMatrix, length (pixelMatrix !! 0))

        chunkSize = getChunkSize width desiredSize 
    
        chunkedImage = chunkPixelMatrix pixelMatrix height chunkSize 0

        listOfChunks = getListOfChunks chunkedImage desiredSize chunkSize 0 0

        downsampledChunks = map getNNDownsampledPixel listOfChunks 
    in listToMatrix desiredSize downsampledChunks

