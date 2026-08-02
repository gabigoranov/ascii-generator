module Ascii.Downsampling.NearestNeighbour (
    downsampleNN
) where

import Codec.Picture

-- Calculates the desired downsampling grid width and height
getChunkSize :: Int -> Int -> Int
getChunkSize size desiredSize = 
    size `div` desiredSize 


-- Extract the top left most pixel in a grid
getDownsampledPixel :: [[PixelRGB8]] -> PixelRGB8
getDownsampledPixel pixelMatrix = (pixelMatrix !! 0) !! 0

-- Converts the JuicyPixels image to a useable matrix
imageToNestedList :: Image PixelRGB8 -> [[PixelRGB8]]
imageToNestedList img = 
    [ [ pixelAt img x y | x <- [0 .. imageWidth img - 1] ] 
    | y <- [0 .. imageHeight img - 1] 
    ]

-- Convert a row of pixels into chunks
chunkPixelArray :: [PixelRGB8] -> Int -> [[PixelRGB8]]
chunkPixelArray pixels chunkSize =
    if length pixels < chunkSize
        then []  -- Return nothing/empty list if it is too small
        else (take chunkSize pixels) : chunkPixelArray (drop chunkSize pixels) chunkSize

-- Returns a list of chunked pixel arrays 
chunkImage :: [[PixelRGB8]] -> Int -> Int -> Int -> [[[PixelRGB8]]]
chunkImage pixelMatrix rows chunkSize currRow =
    if currRow >= rows
        then []
        else
            let currPixelRow = pixelMatrix !! currRow

                chunkedRow = chunkPixelArray currPixelRow chunkSize

            in chunkedRow : chunkImage pixelMatrix rows chunkSize ( currRow + 1 )

generateListOfChunks :: [[[PixelRGB8]]] -> Int -> Int -> Int -> Int -> [[[PixelRGB8]]]
generateListOfChunks flatChunkedImage chunkCols chunkSize currChunk currRow =
    if currChunk >= chunkCols
        then 
            if currRow >= (length flatChunkedImage) - chunkSize
                then []
                
                else generateListOfChunks flatChunkedImage chunkCols chunkSize 0 (currRow + chunkSize)
        else
            let chunkedRows = take chunkSize flatChunkedImage
                heads = map (\row -> row !! currChunk) chunkedRows

            in heads : generateListOfChunks flatChunkedImage chunkCols chunkSize (currChunk + 1) currRow

listToMatrix :: Int -> [a] -> [[a]]
listToMatrix chunkSize [] = [] -- Base Case: An empty list returns an empty matrix
listToMatrix chunkSize xs = 
    -- Grab the first row, then recursively process the rest of the list
    take chunkSize xs : listToMatrix chunkSize (drop chunkSize xs)

matrixToImage :: [[PixelRGB8]] -> Image PixelRGB8
matrixToImage pixelMatrix = generateImage generatePixel width height
  where
    -- 1. Determine the height (rows) and width (cols) of your matrix
    height = length pixelMatrix
    width  = if height > 0 then length (pixelMatrix !! 0) else 0

    -- 2. Grab the pixel at row 'y' and column 'x'
    generatePixel x y = (pixelMatrix !! y) !! x

downsampleNN :: Image PixelRGB8 -> Int -> IO (Image PixelRGB8)
downsampleNN image desiredSize = do
    let pixelMatrix = imageToNestedList image

    let (height, width) = (length pixelMatrix, length (pixelMatrix !! 0))

    let chunkSize = getChunkSize width desiredSize 
    
    putStrLn ("Downsampling to " ++ show desiredSize ++ " px with chunk size of " ++ show chunkSize ++ " px")

    let chunkedImage = chunkImage pixelMatrix height chunkSize 0

    let listOfChunks = generateListOfChunks chunkedImage desiredSize chunkSize 0 0

    let downsampledChunks = map getDownsampledPixel listOfChunks 

    let downsampledMatrix = listToMatrix desiredSize downsampledChunks

    return ( matrixToImage downsampledMatrix )




