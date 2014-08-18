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
import System.FilePath ((</>), replaceExtension)
import System.Environment
import System.Console.GetOpt
import Data.Text (pack, replace, unpack)
import Text.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

data OptionFlag = Verbose  | Version
  deriving (Show, Eq)
   
type SrcDir = FilePath
type DstDir = FilePath
type SrcPath = FilePath
type DstPath = FilePath

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

-- | Return the destination path for a given source path
dstPath :: SrcDir -> DstDir -> SrcPath -> DstPath
dstPath srcDir dstDir = unpack . (replace srcText dstText) . pack . tohtml
  where
    srcText = pack srcDir
    dstText = pack dstDir
    tohtml fp = replaceExtension fp ".html"

-- | Transform a list of source (markdown) paths to a list of dstPath (html) paths
destPaths :: SrcDir -> DstDir -> [SrcPath] -> [DstPath]
destPaths srcDir dstDir srcPaths = fmap src2dst srcPaths where
  src2dst = dstPath srcDir dstDir

-- | Augment a list of source (markdown) paths with dstPath (html) paths
srcdestPairs :: SrcDir -> DstDir -> [SrcPath] -> [(SrcPath, DstPath)]
srcdestPairs srcDir dstDir srcPaths = fmap addDestTo srcPaths where
  src2dst = dstPath srcDir dstDir
  addDestTo sfp = (sfp, src2dst sfp) 

-- | Use default pandoc read and write options
rOptions = def
wOptions = def

-- | Is it possible to stream this ?
runPandoc :: String -> String
runPandoc md =
  writeHtmlString wOptions $ readMarkdown rOptions md


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

