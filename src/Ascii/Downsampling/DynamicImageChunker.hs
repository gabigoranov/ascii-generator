module Ascii.Downsampling.DynamicImageChunker (
    getChunkCoverage,
    getBoundaries,
    chunkPixelList,
    chunkPixelMatrix,
    getListOfChunks
) where

import Codec.Picture

-- Calculates the floating point number of pixels ach chunk needs to consider
getChunkCoverage :: Int -> Int -> Float
getChunkCoverage width desiredWidth =
    fromIntegral width / fromIntegral desiredWidth 

-- Creates a lis tof boundaries based on the image size and avgChunkCoverage 
-- Example Boundaries: 0, 3, 7, etc. -> (0 -> 3), (4 -> 7), (8 -> ...)
getBoundaries :: Int -> Float -> Float -> [Int]
getBoundaries size chunkCoverage idx =
    let currentBoundary :: Int
        currentBoundary = round ( idx * chunkCoverage ) :: Int 

        nextIdx = idx + 1

    in if (nextIdx * chunkCoverage > fromIntegral size)
        then [currentBoundary]
        else currentBoundary : getBoundaries size chunkCoverage nextIdx 

getChunkSizeFromBoundary :: [Int] -> Int -> Int
getChunkSizeFromBoundary boundaries chunkIdx = 
    let pixelsUpToBoundaryCount = boundaries !! ( chunkIdx + 1 )
        pixelsBeforeChunkBoundaryCount = boundaries !! chunkIdx
    in pixelsUpToBoundaryCount - pixelsBeforeChunkBoundaryCount

-- Convert a row of pixels into chunks
chunkPixelList :: [PixelRGB8] -> [Int] -> Int -> [[PixelRGB8]]
chunkPixelList pixels boundaries idx = 
    if (idx + 1) >= length boundaries
        then []
        else 
            let
                pixelsUpToBoundaryCount = boundaries !! ( idx + 1 )
                pixelsUpToBoundary = take pixelsUpToBoundaryCount pixels

                chunk = drop ( boundaries !! idx ) pixelsUpToBoundary 

            in chunk : chunkPixelList pixels boundaries ( idx + 1 )

-- Returns a list of chunked pixel arrays 
-- Example: [1, 1, 1, 1, 1], -> [[1, 1], [1, 1, 1]],
--          [1, 1, 1, 1, 1],    [[1, 1], [1, 1, 1]],
--          [1, 1, 1, 1, 1],    [[1, 1], [1, 1, 1]],
--          [1, 1, 1, 1, 1]     [[1, 1], [1, 1, 1]]
chunkPixelMatrix :: [[PixelRGB8]] -> Int -> [Int] -> Int -> [[[PixelRGB8]]]
chunkPixelMatrix pixelMatrix rows boundaries currRow =
    if currRow >= rows
        then []
        else
            let currPixelRow = pixelMatrix !! currRow

                chunkedRow = chunkPixelList currPixelRow boundaries 0

            in chunkedRow : chunkPixelMatrix pixelMatrix rows boundaries ( currRow + 1 )

-- Returns a one dimensional list of chunks in a specified size ( Example: [2x2], [3x3], etc. )
getListOfChunks :: [[[PixelRGB8]]] -> [Int] -> Int -> Int -> [[[PixelRGB8]]]
getListOfChunks chunkedByRowMatrix boundaries currChunkIdx currRow =
    let 
        totalChunksCount = length boundaries - 1
    in 
        if currChunkIdx >= totalChunksCount -- if we have ran out of chunks on the row
            then 
                let chunkSize = getChunkSizeFromBoundary boundaries 1 -- go down by the amount for the first chunk in the next row
                in if currRow > ((length chunkedByRowMatrix) - chunkSize)
                    then []
                    else getListOfChunks chunkedByRowMatrix boundaries 0 (currRow + chunkSize)
            else
                let chunkSize = getChunkSizeFromBoundary boundaries currChunkIdx
                    remainingRows = drop currRow chunkedByRowMatrix -- slice only the remaining rows to not get stuck
                    chunkedRows = take chunkSize remainingRows
                    heads = map (\row -> row !! currChunkIdx) chunkedRows
                in heads : getListOfChunks chunkedByRowMatrix boundaries (currChunkIdx + 1) currRow


