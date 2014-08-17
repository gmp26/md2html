{-# LANGUAGE NoImplicitPrelude #-}

module Main where

-- we'll use Foldable and Traversable instead of the Prelude defs
import Prelude hiding (
  forM , forM_ , mapM , mapM_ , msum , sequence , sequence_ , concat, elem, notElem)
import  Data.List hiding ( 
  all , and , any , concat , concatMap , elem , filter ,
  find , foldl , foldl' , foldl1 , foldr , foldr1 ,
  mapAccumL , mapAccumR , maximum , maximumBy , minimum , 
  minimumBy , notElem , or , product , sum )
import Data.Foldable
import Data.Traversable
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath.Glob (globDir1, compile)
import System.FilePath ((</>))
import System.Environment
import System.Console.GetOpt
import Data.Text (pack, replace, unpack)


data OptionFlag = Verbose  | Version
  deriving (Show, Eq)
   
type SrcDir = FilePath
type DstDir = FilePath

options :: [OptDescr OptionFlag]
options = 
  [ Option ['V'] ["version"] (NoArg Version)      "show version number",
    Option ['v'] ["verbose"] (NoArg Verbose)      "chatty output on stderr"
  ]

-- | Fetch and parse command line args
processOpts :: [String] -> IO ([OptionFlag], SrcDir, DstDir)
processOpts argv = 
  case getOpt Permute options argv of
     --(o,n,[]  ) -> return (o,n)
     (o,[],[]) -> return (o,"md/","html/")
     (o,srcDir:[],[]) -> return (o,srcDir,"html/")
     (o,srcDir:dstDir:xs,[]) -> return (o,srcDir,dstDir)

     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: md2html [OPTION...] srcdir dstdir"


-- | Apply glob pattern to yield a list of markdown files in a directory
markdownPaths :: FilePath -> IO [FilePath]
markdownPaths = globDir1 (compile "**/*.md")


-- | Transform a list of source (markdown) paths to a list of destination (html) paths
destPaths :: SrcDir -> DstDir -> [FilePath] -> [FilePath]
destPaths srcDir dstDir srcPaths = fmap src2dst srcPaths
  where
    src2dst fp = 
      unpack $ replace srcText dstText (pack fp)
    srcText = pack srcDir
    dstText = pack dstDir


-- | Walk a directory, selecting files, and processing
mapDir :: (FilePath -> IO ()) -> FilePath -> IO ()
mapDir proc fp = do
  isFile <- doesFileExist fp -- is a file of fp
  if isFile then proc fp     -- process the file
  else getDirectoryContents fp >>=
       mapM_ (mapDir proc . (fp </>)) . filter (`notElem` [".", ".."])


main :: IO()
main = do

  argv <- getArgs
  (userOpts, srcdir, dstdir) <- processOpts argv

  putStrLn $ if Version `elem` userOpts
    then "version 0.1.0.0"
    else ""

  putStrLn $ "srcdir = " ++ srcdir
  putStrLn $ "dstdir = " ++ dstdir

  srcFiles <- markdownPaths srcdir
  mapM_ putStrLn srcFiles

  let dstFiles = destPaths srcdir dstdir srcFiles
  mapM_ putStrLn dstFiles

