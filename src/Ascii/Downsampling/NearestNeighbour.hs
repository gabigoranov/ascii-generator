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
downsampleNN :: Image PixelRGB8 -> Int -> IO (Image PixelRGB8)
downsampleNN image desiredSize = do
    let pixelMatrix = juicyToMatrix image

    let (height, width) = (length pixelMatrix, length (pixelMatrix !! 0))

    let chunkSize = getChunkSize width desiredSize 
    
    putStrLn ("Downsampling to " ++ show desiredSize ++ " px with chunk size of " ++ show chunkSize ++ " px")

    let chunkedImage = chunkPixelMatrix pixelMatrix height chunkSize 0

    let listOfChunks = getListOfChunks chunkedImage desiredSize chunkSize 0 0

    let downsampledChunks = map getNNDownsampledPixel listOfChunks 

    let downsampledMatrix = listToMatrix desiredSize downsampledChunks

    return ( matrixToImage downsampledMatrix )




