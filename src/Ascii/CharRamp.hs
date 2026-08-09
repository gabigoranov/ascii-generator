{-# LANGUAGE DeriveDataTypeable #-}
module Ascii.CharRamp (
    asciiRamp,
    detailedAsciiRamp,
    AsciiRamp(..)
) where

import System.Console.CmdArgs

asciiRamp :: String
asciiRamp = "@%#*+=-:. "

detailedAsciiRamp :: String
detailedAsciiRamp = "@$&%#*+=-:. "

data AsciiRamp = Standart | Detailed
    deriving (Show, Read, Eq, Bounded, Enum, Data, Typeable)

