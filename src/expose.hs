import Control.Monad (filterM)
import Data.List (elemIndex)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>), makeRelative, splitDirectories, takeExtensions)

main :: IO ()
main = do
    args <- getArgs
    let searchDir   = getSearchDir args
        isRecursive = "-r" `elem` args
        filterByExt = filter (getExtensionFilter args)
    dirContents <- getDirContents searchDir
    files       <- if isRecursive
                     then recursiveGetFiles searchDir dirContents
                     else getFiles dirContents
    putStrLn . unlines . map (makeRelative searchDir) . filterByExt $ files

getSearchDir :: [String] -> FilePath
getSearchDir args = case elemIndex "-d" args of
                        Just i  -> args !! (i + 1)
                        Nothing -> "."

getExtensionFilter :: [String] -> (FilePath -> Bool)
getExtensionFilter args = case elemIndex "-e" args of
                              Just i  ->
                                  \fp -> takeExtensions fp == args !! (i + 1)
                              Nothing -> const True

getDirContents :: FilePath -> IO [FilePath]
getDirContents dir = return . map (dir </>) =<< getDirectoryContents dir

getFiles :: [FilePath] -> IO [FilePath]
getFiles = filterM doesFileExist

getDirs :: [FilePath] -> IO [FilePath]
getDirs = filterM isDir
    where
        isDir dir = if lastDir dir `elem` [".", ".."]
                      then return False
                      else doesDirectoryExist dir
        lastDir   = last . splitDirectories

recursiveGetFiles :: FilePath -> [FilePath] -> IO [FilePath]
recursiveGetFiles searchDir filePaths = do
    files       <- getFiles filePaths
    dirs        <- getDirs filePaths
    dirContents <- mapM getDirContents dirs
    moreFiles   <- mapM (recursiveGetFiles searchDir) dirContents
    return $ files ++ concat moreFiles
