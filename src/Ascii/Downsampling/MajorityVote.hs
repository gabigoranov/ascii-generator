module Ascii.Downsampling.MajorityVote (
    downsampleMV
) where

import Codec.Picture
import Ascii.Downsampling.ImageConverter
import Ascii.Downsampling.ImageChunker
import Ascii.PerceivedBrightness (normalizePerceivedBrightness, calculatePerceivedBrightness)

import Data.Ord (comparing)
import Data.List ( maximumBy, elemIndex, group, sort )

-- Helper: Assigns a brightness value to a specific bucket (e.g., 0.13 becomes 1)
toBucket :: Float -> Int
toBucket v = floor (v / 0.1)

-- Count how many items are in each bucket
countBuckets :: [Float] -> [(Int, Int)]
countBuckets values = 
    let bucketList = sort (map toBucket values)               -- [0, 0, 1, 1, 1, 2]
        grouped    = group bucketList                         -- [[0,0], [1,1,1], [2]]
    in map (\g -> (head g, length g)) grouped                 -- [(0,2), (1,3), (2,1)]

-- Find the bucket with the highest count (the majority)
findMajority :: [(Int, Int)] -> (Int, Int)
findMajority counts = maximumBy (comparing snd) counts

-- Finds the first occurance of a perceived brightness in a list of buckets
findFirstBucketIndex :: Int -> [Int] -> Maybe Int
findFirstBucketIndex targetBucket bucketList = elemIndex targetBucket bucketList

getMVDownsampledPixel :: [[PixelRGB8]] -> PixelRGB8
getMVDownsampledPixel pixelMatrix =
    let flattened = concat pixelMatrix

        perceivedBrightnessMap = map ( normalizePerceivedBrightness . calculatePerceivedBrightness ) flattened
       
        bucketsMap = map toBucket perceivedBrightnessMap 
       
        (majorityBucket, _) = findMajority ( countBuckets perceivedBrightnessMap )
       
        maybeIndexOfMajorityBucket = findFirstBucketIndex majorityBucket bucketsMap 
       
        finalIndex = 
           case maybeIndexOfMajorityBucket of
               Just idx -> idx
               Nothing -> 0 -- While testing return a fallback to the first pixel

    in flattened !! finalIndex 


downsampleMV :: Image PixelRGB8 -> Int -> IO (Image PixelRGB8)
downsampleMV image desiredSize = do
    let pixelMatrix = juicyToMatrix image

    let (height, width) = (length pixelMatrix, length (pixelMatrix !! 0))

    let chunkSize = getChunkSize width desiredSize 

    putStrLn ("Downsampling to " ++ show desiredSize ++ " px with chunk size of " ++ show chunkSize ++ " px")

    let chunkedImage = chunkPixelMatrix pixelMatrix height chunkSize 0

    let listOfChunks = getListOfChunks chunkedImage desiredSize chunkSize 0 0

    let downsampledChunks = map getMVDownsampledPixel listOfChunks 

    let downsampledMatrix = listToMatrix desiredSize downsampledChunks

    return ( matrixToImage downsampledMatrix )
