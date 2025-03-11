module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
import System.Directory (doesFileExist)

data Command = Curate | Tag
    deriving (Show, Eq)

parseCommand :: String -> Maybe Command
parseCommand "curate" = Just Curate
parseCommand "tag" = Just Tag
parseCommand _ = Nothing

data MP3Info = MP3Info 
    { songName :: String
    , authorName :: String
    } deriving Show

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> die "Error: Command (curate|tag) and absolute path to MP3 file are required"
        [_] -> die "Error: Absolute path to MP3 file is required"
        (cmd:filePath:_) -> 
            case parseCommand cmd of
                Nothing -> die "Error: Command must be either 'curate' or 'tag'"
                Just command -> validatePath filePath

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
        _ -> do
            let dirs = splitDirectories path
                fileName = takeBaseName (last dirs)  -- obtiene el nombre del archivo sin extensión
                author = last (init dirs)           -- obtiene el directorio padre
                mp3Info = MP3Info fileName author
            print mp3Info
