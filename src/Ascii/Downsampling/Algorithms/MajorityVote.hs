module Ascii.Downsampling.Algorithms.MajorityVote (
    downsampleMV
) where

import Codec.Picture
import Ascii.Downsampling.ImageConverter
import Data.Ord (comparing)
import Data.List ( maximumBy, elemIndex, group, sort )
import Ascii.RelativeLuminance (normalizeLuma, getLuma)

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
        perceivedBrightnessMap = map ( normalizeLuma . getLuma ) flattened
        bucketsMap = map toBucket perceivedBrightnessMap 

        (majorityBucket, _) = findMajority ( countBuckets perceivedBrightnessMap )
        maybeIndexOfMajorityBucket = findFirstBucketIndex majorityBucket bucketsMap 

        finalIndex = 
           case maybeIndexOfMajorityBucket of
               Just idx -> idx
               Nothing -> 0 -- fallback to the first pixel

    in flattened !! finalIndex 


downsampleMV :: [[[PixelRGB8]]] -> Int -> [[PixelRGB8]]
downsampleMV listOfChunks desiredSize =
    let downsampledChunks = map getMVDownsampledPixel listOfChunks 
    in listToMatrix desiredSize downsampledChunks
