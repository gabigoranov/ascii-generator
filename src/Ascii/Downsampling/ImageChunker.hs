module Ascii.Downsampling.ImageChunker (
    getChunkSize,
    chunkPixelList,
    chunkPixelMatrix,
    getListOfChunks
) where

import Codec.Picture

-- Calculates the desired downsampling grid width and height
getChunkSize :: Int -> Int -> Int
getChunkSize size desiredSize = 
    size `div` desiredSize 

-- Convert a row of pixels into chunks
chunkPixelList :: [PixelRGB8] -> Int -> [[PixelRGB8]]
chunkPixelList pixels chunkSize =
    if length pixels < chunkSize
        then []  -- Return nothing/empty list if it is too small
        else (take chunkSize pixels) : chunkPixelList (drop chunkSize pixels) chunkSize

-- Returns a list of chunked pixel arrays 
-- Example: [1, 1, 1, 1], -> [[1, 1], [1, 1]],
--          [1, 1, 1, 1],    [[1, 1], [1, 1]],
--          [1, 1, 1, 1],    [[1, 1], [1, 1]],
--          [1, 1, 1, 1]     [[1, 1], [1, 1]]
chunkPixelMatrix :: [[PixelRGB8]] -> Int -> Int -> Int -> [[[PixelRGB8]]]
chunkPixelMatrix pixelMatrix rows chunkSize currRow =
    if currRow >= rows
        then []
        else
            let currPixelRow = pixelMatrix !! currRow

                chunkedRow = chunkPixelList currPixelRow chunkSize

            in chunkedRow : chunkPixelMatrix pixelMatrix rows chunkSize ( currRow + 1 )

-- Returns a one dimensional list of chunks in a specified size ( Example: [2x2], [2x2], etc. )
getListOfChunks :: [[[PixelRGB8]]] -> Int -> Int -> Int -> Int -> [[[PixelRGB8]]]
getListOfChunks flatChunkedImage chunkCols chunkSize currChunk currRow =
    if currChunk >= chunkCols
        then 
            if currRow >= (length flatChunkedImage) - chunkSize
                then []
                
                else getListOfChunks flatChunkedImage chunkCols chunkSize 0 (currRow + chunkSize)
        else
            let chunkedRows = take chunkSize flatChunkedImage
                heads = map (\row -> row !! currChunk) chunkedRows

            in heads : getListOfChunks flatChunkedImage chunkCols chunkSize (currChunk + 1) currRow

