module InputHelper (
    prompt,
    getOutputSize,
    getAlgorithm,
    getAsciiRamp 
) where
import System.IO
import Ascii.CharRamp (detailedAsciiRamp, asciiRamp)

-- Prompt helper that flushes stdout immediately so text appears before input
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- Safely get and validate the output pixel size limit
getOutputSize :: IO Int
getOutputSize = do
    input <- prompt "Enter output size (max 128 pixels): "
    case reads input :: [(Int, String)] of
        [(size, "")] -> 
            if size > 0 && size <= 128
                then return size
                else do
                    putStrLn "Error: Size must be between 1 and 128."
                    getOutputSize
        _ -> do
            putStrLn "Error: Please enter a valid integer."
            getOutputSize

-- Safely get the algorithm selection (including the "No Downsampling" option)
getAlgorithm :: IO String
getAlgorithm = do
    putStrLn "Choose an operation mode:"
    putStrLn "1) Nearest Neighbour Downsampling (NN)"
    putStrLn "2) Majority Vote Downsampling (MV)"
    putStrLn "3) No Downsampling (Use Original Size)"
    choice <- prompt "Enter choice (1, 2, or 3): "
    if choice `elem` ["1", "2", "3"]
        then return choice
        else do
            putStrLn "Error: Invalid choice."
            getAlgorithm

-- Safely get and validate the detail selection
getAsciiRamp :: IO String
getAsciiRamp = do
    putStrLn "Choose a level of detail:"
    putStrLn "1) Standart - \"@%#*+=-:.\""
    putStrLn "2) Detailed - \"@$&%#*+=-:. \""
    choice <- prompt "Enter choice (1 or 2): "

    if choice `elem` ["1", "2"]
        then 
            if choice == "1"
                then return asciiRamp 
                else return detailedAsciiRamp 

        else do
            putStrLn "Error: Invalid choice."
            getAsciiRamp 
