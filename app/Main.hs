{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative        ((<|>))
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson                 hiding (encode)
import qualified Data.Text                  as T
import           Network.HTTP.Simple
import           Network.URI.Encode         (encode)
import           System.Directory           (doesFileExist, renameFile)
import           System.Environment         (getArgs)
import           System.Exit                (die)
import           System.FilePath
import           Text.Read                  (readMaybe)
import           Text.Parsec                hiding ((<|>))
import           Text.Parsec.String         (Parser)
import           Sound.HTagLib

data Command = Curate | Tag
    deriving (Show, Eq)

parseCommand :: String -> Maybe Command
parseCommand "curate" = Just Curate
parseCommand "tag"    = Just Tag
parseCommand _        = Nothing

data MP3Info = MP3Info
    { songName   :: String
    , authorName :: String
    } deriving Show

data ValidationError
    = NotAbsolutePath
    | NoParentFolder
    | NotMP3Extension
    | FileNotFound FilePath
    deriving Show

data KbpsPattern = KbpsPattern
    { openBracket  :: Char
    , bitrate      :: Int
    , closeBracket :: Char
    } deriving Show

data ValidatedArgs = ValidatedArgs Command FilePath

data CommandLineError
    = NoArgs
    | NoFilePath
    | InvalidCommand
    deriving Show

showCommandLineError :: CommandLineError -> String
showCommandLineError = \case
    NoArgs -> "Command (curate|tag) and absolute path to MP3 file are required"
    NoFilePath -> "Absolute path to MP3 file is required"
    InvalidCommand -> "Command must be either 'curate' or 'tag'"

kbpsParser :: Parser KbpsPattern
kbpsParser = do
    spaces  -- consume leading spaces
    open <- char '(' <|> char '['
    spaces
    rate <- read <$> many1 digit
    spaces
    _ <- string "kbps" <|> string "Kbps" <|> string "KBPS"
    spaces
    close <- char ')' <|> char ']'
    return $ KbpsPattern open rate close

showValidationError :: ValidationError -> String
showValidationError = \case
    NotAbsolutePath -> "The path must be absolute"
    NoParentFolder -> "The file should be under a directory"
    NotMP3Extension -> "Expected extension to be .mp3"
    FileNotFound path -> "File not found: " ++ path

songNameParser :: Parser (String, Maybe KbpsPattern)
songNameParser = do
    name <- many1 (noneOf "([")  -- capture everything until a bracket
    kbps <- optionMaybe (try kbpsParser)
    return (name, kbps)

replaceKbpsPattern :: String -> String
replaceKbpsPattern input =
    case parse songNameParser "" input of
        Left _          -> input  -- if parsing fails, return original input
        Right (name, _) -> trim name
  where
    trim = T.unpack . T.strip . T.pack

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
    if newPath /= originalPath
        then do
            renameFile originalPath newPath
            putStrLn $ "File renamed from: " ++ originalPath ++ " to: " ++ newPath
        else putStrLn "No curation needed"

validatePath :: FilePath -> IO (Either ValidationError MP3Info)
validatePath path = runExceptT $ do
    unless (isAbsolute path) $
        throwE NotAbsolutePath

    unless (length (splitDirectories path) > 1) $
        throwE NoParentFolder

    unless (takeExtension path == ".mp3") $
        throwE NotMP3Extension

    exists <- liftIO $ doesFileExist path
    unless exists $
        throwE (FileNotFound path)

    let dirs = splitDirectories path
        fileName = takeBaseName (last dirs)
        author = last (init dirs)

    return $ MP3Info fileName author

validateArgs :: [String] -> Either CommandLineError ValidatedArgs
validateArgs [] = Left NoArgs
validateArgs [_] = Left NoFilePath
validateArgs (cmd:filePath:_) =
    case parseCommand cmd of
        Nothing      -> Left InvalidCommand
        Just command -> Right $ ValidatedArgs command filePath

executeCommand :: ValidatedArgs -> IO ()
executeCommand (ValidatedArgs command filePath) = do
    result <- validatePath filePath
    case result of
        Left err      -> die $ "Error: " ++ showValidationError err
        Right mp3Info ->
          case command of
              Curate -> curateMP3 mp3Info filePath
              Tag -> do
                x <- searchMusicBrainz (songName mp3Info) (authorName mp3Info)
                case x of
                    Left err -> die $ "Error: " ++ err
                    Right track -> do
                        putStrLn $ "Title: " ++ mbTitle track
                        putStrLn $ "Artist: " ++ mbArtist track
                        putStrLn $ "Release Date: " ++ show (mbReleaseDate track)
                        putStrLn $ "Genres: " ++ unwords (mbGenres track)
                        tagMP3 filePath track
                        putStrLn "Done!"

-- MusicBrainz API
data MusicBrainzTrack = MusicBrainzTrack
    { mbTitle       :: String
    , mbArtist      :: String
    , mbReleaseDate :: Maybe Int
    , mbGenres      :: [String]
    } deriving (Show, Eq)

instance FromJSON MusicBrainzTrack where
    parseJSON = withObject "MusicBrainzTrack" $ \obj -> do
        recordings <- obj .: "recordings"
        case recordings of
            [] -> fail "No recordings found"
            (recording:_) -> do
                title <- recording .: "title"

                -- Get artist from artist-credit array
                artistCredits <- recording .: "artist-credit"
                artist <- case artistCredits of
                    []         -> fail "No artist credits found"
                    (credit:_) -> credit .: "name"

                -- Get release date from releases array
                releases <- recording .:? "releases" .!= []
                releaseDate <- case releases of
                    []          -> return "Unknown"
                    (release:_) -> release .:? "date" .!= "Unknown"

                -- Get genres from tags array
                tags <- recording .:? "tags" .!= []
                genres <- sequence $ map (\tag -> tag .: "name") tags

                return $ MusicBrainzTrack
                    { mbTitle = title
                    , mbArtist = artist
                    , mbReleaseDate = readMaybe releaseDate
                    , mbGenres = genres
                    }

searchMusicBrainz :: String -> String -> IO (Either String MusicBrainzTrack)
searchMusicBrainz title artist = do
    let query = encode $ "recording:" ++ title ++ " AND artist:" ++ artist
        url = "https://musicbrainz.org/ws/2/recording/?query=" ++ query ++ "&fmt=json"

    request <- parseRequest url

    let requestWithHeaders = setRequestHeader "User-Agent" ["TM/0.1.0 (https://github.com/CristhianMotoche/tm)"] request

    response <- httpLBS requestWithHeaders
    return $ case getResponseStatusCode response of
        200 -> case decode (getResponseBody response) of
            Just track -> Right track
            Nothing    -> Left "Failed to parse MusicBrainz response"
        code -> Left $ "MusicBrainz API error: " ++ show code ++ show (getResponseBody response)

tagMP3 :: FilePath -> MusicBrainzTrack -> IO ()
tagMP3 filePath track =
    setTags filePath Nothing $
        titleSetter (mkTitle $ T.pack $ mbTitle track) <>
        artistSetter (mkArtist $ T.pack $ mbArtist track) <>
        maybe mempty (yearSetter . mkYear) (mbReleaseDate track) <>
        genreSetter (mkGenre $ T.pack $ unwords $ mbGenres track)

main :: IO ()
main = do
    args <- getArgs
    case validateArgs args of
        Left err            -> die $ "Error: " ++ showCommandLineError err
        Right validatedArgs -> executeCommand validatedArgs
