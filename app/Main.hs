module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
import System.Directory (doesFileExist)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> die "Error: Absolute path to MP3 file is required"
        (filePath:_) -> validatePath filePath

validatePath :: FilePath -> IO ()
validatePath path = do
    let isAbsolute' = isAbsolute path
        hasFolder = length (splitDirectories path) > 1
        hasMP3Ext = takeExtension path == ".mp3"
    
    fileExists <- doesFileExist path
    
    case (isAbsolute', hasFolder, hasMP3Ext, fileExists) of
        (False, _, _, _) -> 
            die "Error: The path must be absolute"
        (_, False, _, _) -> 
            die "Error: The file should be under a directory"
        (_, _, False, _) -> 
            die "Error: Expected extension to be .mp3"
        (_, _, _, False) -> 
            die $ "Error: File not found: " ++ path
        _ -> putStrLn $ "Not a valid route: " ++ path
