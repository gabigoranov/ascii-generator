module Ascii.ImageParser (
    getRawImageInfo,
    parseRawImageInfo,
    getProportionalHeight 
) where

import qualified Codec.Picture.Metadata as Metadata

-- Extracts the raw, possibly null, Width and Height
getRawImageInfo :: Metadata.Metadatas -> (Maybe Word, Maybe Word)
getRawImageInfo meta = 
    let width  = Metadata.lookup Metadata.Width meta
        height = Metadata.lookup Metadata.Height meta
    in (width, height)

-- Returns an error message if the Width and Height metadata does not exist
parseRawImageInfo :: (Maybe Word, Maybe Word) -> ( Either String ( Int, Int)  )
parseRawImageInfo (rawWidth, rawHeight) = 
    case ( rawWidth, rawHeight ) of
        (Just w, Just h ) -> Right (fromIntegral w, fromIntegral h)
        (Just _, Nothing ) -> Left "Unknown image height!"
        (Nothing, Just _ ) -> Left "Unknown image width!"
        (Nothing, Nothing) -> Left "Unknown image dimensions!"

-- Scales the output height to preserve the image's aspect ratio
getProportionalHeight :: Int -> Int -> Int -> Int
getProportionalHeight width height desiredWidth =
    round ( ( fromIntegral desiredWidth * fromIntegral height / fromIntegral width ) :: Double)

