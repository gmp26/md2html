module Main where

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath.Glob (globDir1, compile)
import System.FilePath ((</>))

-- Apply glob pattern to yield a list of markdown files in a directory
markdownFiles :: FilePath -> IO [FilePath]
markdownFiles dir = globDir1 (compile "**/*.md") dir

process :: FilePath -> IO ()
process = putStrLn

-- | Walk a directory, selecting files, and processing
mapDir :: (FilePath -> IO ()) -> FilePath -> IO ()
mapDir proc fp = do
  isFile <- doesFileExist fp -- is a file of fp
  if isFile then proc fp     -- process the file
  else getDirectoryContents fp >>=
       mapM_ (mapDir proc . (fp </>)) . filter (`notElem` [".", ".."])

-- main = mapDir process "."


main :: IO()
main = do
  putStrLn "Hi"
  files <- markdownFiles ".."
  putStrLn $ show files