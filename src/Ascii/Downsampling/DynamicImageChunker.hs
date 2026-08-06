module Ascii.Downsampling.DynamicImageChunker (
    getChunkCoverage,
    getBoundaries,
    chunkPixelMatrixHorizontally,
    chunkPixelList,
    getListOfChunks,
    chunkPixelMatrix
) where

import Codec.Picture

-- Calculates the floating point number of pixels ach chunk needs to consider
-- Can be used for chunk width or height calculation
getChunkCoverage :: Int -> Int -> Float
getChunkCoverage size desiredSize =
    fromIntegral size / fromIntegral desiredSize 

-- Creates a lis tof boundaries based on the image size and avgChunkCoverage 
-- Can be used for horizontal and vertical boundaries
-- Example Boundaries: 0, 3, 7, etc. -> (0 -> 3), (4 -> 7), (8 -> ...)
getBoundaries :: Int -> Float -> Float -> [Int]
getBoundaries size chunkCoverage idx =
    let currentBoundary :: Int
        currentBoundary = round ( idx * chunkCoverage ) -- start boundary from 0 

        nextIdx = idx + 1

    in  if (nextIdx * chunkCoverage > fromIntegral size)
            then [currentBoundary]
            else currentBoundary : getBoundaries size chunkCoverage nextIdx 

-- Calculages the chunk size ( wdith or height based on the given boundaries )
getChunkSizeFromBoundary :: [Int] -> Int -> Int
getChunkSizeFromBoundary _ 0 = 0
getChunkSizeFromBoundary boundaries idx = 
    let pixelsUpToBoundaryCount = boundaries !! idx
        pixelsBeforeChunkBoundaryCount = boundaries !! ( idx - 1 )
    in pixelsUpToBoundaryCount - pixelsBeforeChunkBoundaryCount

-- Convert a row of pixels into chunks
chunkPixelList :: [PixelRGB8] -> [Int] -> Int -> [[PixelRGB8]]
chunkPixelList pixels horizontalBoundaries idx =
    if (idx + 1) >= length horizontalBoundaries
        then []
        else 
            let pixelsUpToBoundaryCount = horizontalBoundaries !! ( idx + 1 )
                pixelsUpToBoundary = take pixelsUpToBoundaryCount pixels

                chunk = drop ( horizontalBoundaries !! idx ) pixelsUpToBoundary 

            in chunk : chunkPixelList pixels horizontalBoundaries ( idx + 1 )

-- Returns a list of chunked pixel arrays 
-- Example: [1, 1, 1,  1, 1, 1, 1], -> [[1, 1, 1],  [1, 1, 1, 1]], <- vertical group 0 ( later it will be converted into a list of chunks like this )
--
--          [1, 1, 1,  1, 1, 1, 1],    [[1, 1, 1],  [1, 1, 1, 1]], <- vertical group 1
--          [1, 1, 1,  1, 1, 1, 1],    [[1, 1, 1],  [1, 1, 1, 1]],
--          [1, 1, 1,  1, 1, 1, 1],    [[1, 1, 1],  [1, 1, 1, 1]],
--
--          [1, 1, 1,  1, 1, 1, 1]     [[1, 1, 1],  [1, 1, 1, 1]]  <- vertical group 2
chunkPixelMatrixHorizontally :: [[PixelRGB8]] -> [Int] -> Int -> [[[PixelRGB8]]]
chunkPixelMatrixHorizontally pixelMatrix horizontalBoundaries currRow =
    let rows = length pixelMatrix
    in 
        if currRow >= rows
            then []
            else
                let currPixelRow = pixelMatrix !! currRow
                    chunkedRow = chunkPixelList currPixelRow horizontalBoundaries 0

                in chunkedRow : chunkPixelMatrixHorizontally pixelMatrix horizontalBoundaries ( currRow + 1 )

-- Returns a one dimensional list of chunks in a specified size ( Example: [2x1], [3x1], [2x2], [3x2], etc. )
-- Look at chunkPixelMatrixHorizontally
-- TODO: uhh refactor...
getListOfChunks :: [[[PixelRGB8]]] -> [Int] -> [Int] -> Int -> Int -> [[[PixelRGB8]]]
getListOfChunks chunkedByRowMatrix horizontalBoundaries verticalBoundaries horizontalChunkIdx verticalGroupIdx =
    let totalChunkCols = length horizontalBoundaries - 1 
        totalRowsInCurrentVerticalGroup = getChunkSizeFromBoundary verticalBoundaries ( verticalGroupIdx + 1 )

    in  if horizontalChunkIdx >= totalChunkCols -- if we have ran out of chunks on the row
            then 
                if verticalGroupIdx + 1 >= length verticalBoundaries - 1 -- stop when there is no next v group
                    then []
                    else getListOfChunks chunkedByRowMatrix horizontalBoundaries verticalBoundaries 0 (verticalGroupIdx + 1)
            else
                let totalUseableRows = verticalBoundaries !! ( verticalGroupIdx + 1 ) 
                    totalRowsBeforeStartOfVerticalGroup = totalUseableRows - totalRowsInCurrentVerticalGroup 

                    useablePixelRows = take totalUseableRows chunkedByRowMatrix 
                    pixelRowsInCurrentVerticalGroup = drop totalRowsBeforeStartOfVerticalGroup useablePixelRows 

                    heads = map (\row -> row !! horizontalChunkIdx ) pixelRowsInCurrentVerticalGroup 
                in heads : getListOfChunks chunkedByRowMatrix horizontalBoundaries verticalBoundaries (horizontalChunkIdx + 1) verticalGroupIdx

-- Dynamic Chunking logic using separate horizontal and vertical boundaries
chunkPixelMatrix :: [[PixelRGB8]] -> Int -> Int -> [[[PixelRGB8]]]
chunkPixelMatrix matrix desiredWidth desiredHeight = 
    let (height, width) = (length matrix, length (matrix !! 0))

        horizontalChunkCoverage = getChunkCoverage width desiredWidth
        horizontalBoundaries = getBoundaries width horizontalChunkCoverage 0

        verticalGroupCoverage = getChunkCoverage height desiredHeight 
        verticalBoundaries = getBoundaries height verticalGroupCoverage 0

        chunkedByRowMatrix = chunkPixelMatrixHorizontally matrix horizontalBoundaries 0

    in getListOfChunks chunkedByRowMatrix horizontalBoundaries verticalBoundaries 0 0

