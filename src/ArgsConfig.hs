{-# LANGUAGE DeriveDataTypeable #-}
module ArgsConfig 
    ( Config(..)
    , configSpec
    ) where

import System.Console.CmdArgs
import Ascii.CharRamp
import qualified Ascii.CharRamp as AsciiRamp
import Ascii.Downsampling.Algorithms.DownsamplingAlgorithm

data Config = Config 
    { imgPath         :: FilePath -- Kept as a pure FilePath/String!
    , algorithmChoice :: DownsamplingAlgorithm
    , isColoured      :: Bool
    , levelOfDetail   :: AsciiRamp
    , outputWidth     :: Int
    , outputPath      :: Maybe FilePath
    } deriving (Show, Data, Typeable)

configSpec :: Config
configSpec = Config 
    { imgPath         = ""
                        &= help "The path to the image to convert (REQUIRED)" 
                        &= name "imgPath" 
                        &= name "i"       
                        &= typ "FILE"
                        
    , algorithmChoice = NearestNeighbour 
                        &= help "The downsampling algorithm used (NearestNeighbour, MajorityVote)"
                        &= name "algorithmChoice"
                        &= name "a"
                        
    , isColoured      = False 
                        &= help "Whether the art is colourful"
                        &= name "isColoured"
                        &= name "c"
                        
    , levelOfDetail   = AsciiRamp.Standart 
                        &= help "The level of detail (Standart, Detailed)"
                        &= name "levelOfDetail"
                        &= name "l"
                        
    , outputWidth     = 64 
                        &= help "The output width of the art (DEF: 64, MAX: 128)"
                        &= name "outputWidth"
                        &= name "w"
                        &= typ "INT"
                        
    , outputPath      = def 
                        &= help "The export path of the art (DEF: print in terminal)"
                        &= name "outputPath"
                        &= name "o"
                        &= typ "FILE"
    }
    &= summary "ASCII Art Generator v1.0"
    &= program "ascii-generator"
