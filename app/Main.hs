{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
import System.Directory (doesFileExist, renameFile)
import Text.Regex
import Data.List (isSuffixOf)
import qualified Data.Text as T

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
                Just Curate -> do
                   mp3Info <- validatePath filePath
                   curateMP3 mp3Info filePath
                   return ()
                Just Tag -> putStrLn "TBD: Tag file"

replaceKbpsPattern :: String -> String
replaceKbpsPattern input = 
    let pattern = "[ ]*(\\(|\\[)[0-9]+ ?[Kk][Bb]?[Pp][Ss](\\)|\\])"
    in case matchRegexAll (mkRegex pattern) input of
        Just (before, matched, after, _) -> before ++ after
        Nothing -> input

curateMP3 :: MP3Info -> FilePath -> IO ()
curateMP3 mp3info originalPath = do
    let oldName = songName mp3info
        -- Remove patterns like (XXX Kbps), [XXX Kbps], etc
        newName = T.unpack $ T.replace "  " " " $ 
                   T.strip $ 
                   T.pack $ 
                   replaceKbpsPattern oldName
        -- Create new path by replacing old filename with curated one
        directory = takeDirectory originalPath
        newPath = directory </> newName <.> "mp3"
    putStrLn oldName
    putStrLn newName
    if newPath /= originalPath
        then do
            renameFile originalPath newPath
            putStrLn $ "File renamed from: " ++ originalPath
            putStrLn $ "                to: " ++ newPath
        else putStrLn "No curation needed"

validatePath :: FilePath -> IO MP3Info
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
            return mp3Info
