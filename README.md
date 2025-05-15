# TM - MP3 Metadata Curator and Tagger

TM i a command-line tool written in Haskell for curating and tagging MP3 files. It uses the MusicBrainz API to fetch metadata and applies it to MP3 files, making it easier to organize your my library.

## Features

- **Curate MP3 File Names**: Automatically clean up MP3 file names by removing unnecessary patterns like bitrate information.
- **Tag MP3 Files**: Fetch metadata (title, artist, release date, genres) from the MusicBrainz API and apply it to MP3 files.

## Installation

### Prerequisites

- [Devenv](https://github.com/cachix/devenv) for development environment setup

### Run

```
devenv shell
```

### Build

```
cabal build
```
