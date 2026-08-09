{-# LANGUAGE DeriveDataTypeable #-}
module Ascii.Downsampling.Algorithms.DownsamplingAlgorithm
    ( DownsamplingAlgorithm(..)
    ) where

import System.Console.CmdArgs

data DownsamplingAlgorithm = NearestNeighbour | MajorityVote
    deriving (Show, Read, Eq, Bounded, Enum, Data, Typeable)
