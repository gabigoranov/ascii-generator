module Ascii.Downsampling.ImageConverter (
    juicyToMatrix,
    listToMatrix
) where
import Codec.Picture

-- Converts the JuicyPixels image to a useable matrix
juicyToMatrix :: Image PixelRGB8 -> [[PixelRGB8]]
juicyToMatrix img = 
    [ [ pixelAt img x y | x <- [0 .. imageWidth img - 1] ] 
    | y <- [0 .. imageHeight img - 1] 
    ]

-- Converts a flat list of chunks into a matrix based on a given size
-- Example: [2x2], [2x2], [2x2], [2x2] -> [[2x2], [2x2]],
--                                        [[2x2], [2x2]]
listToMatrix :: Int -> [a] -> [[a]]
listToMatrix _ [] = [] -- Base Case: An empty list returns an empty matrix
listToMatrix chunkSize xs = 
    -- Grab the first row, then recursively process the rest of the list
    take chunkSize xs : listToMatrix chunkSize (drop chunkSize xs)
