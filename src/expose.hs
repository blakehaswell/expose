import Control.Monad (filterM)
import Data.List (elemIndex)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>), makeRelative, splitDirectories)

main :: IO ()
main = do
    args <- getArgs
    let searchDir = getSearchDir args
        isRecursive = "-r" `elem` args
    dirContents <- getDirectoryContents searchDir
    files <- if isRecursive then recursiveGetFiles searchDir dirContents
                            else getFiles $ map (searchDir </>) dirContents
    putStrLn . unlines . map (makeRelative searchDir) $ files

getSearchDir :: [String] -> FilePath
getSearchDir args = case elemIndex "-d" args of
                        Just i -> args !! (i + 1)
                        Nothing -> "."

getFiles :: [FilePath] -> IO [FilePath]
getFiles = filterM doesFileExist

getDirs :: [FilePath] -> IO [FilePath]
getDirs = filterM isDir
    where
        isDir dir = if lastDir dir `elem` [".", ".."]
                      then return False
                      else doesDirectoryExist dir
        lastDir = last . splitDirectories

recursiveGetFiles :: FilePath -> [FilePath] -> IO [FilePath]
recursiveGetFiles relativeDir filePaths = do
    dirs <- getDirs $ map (relativeDir </>) filePaths
    moreFiles <- mapM doRecursiveGetFiles dirs
    files <- getFiles $ map (relativeDir </>) filePaths
    return $ files ++ concat moreFiles
        where
            doRecursiveGetFiles :: FilePath -> IO [FilePath]
            doRecursiveGetFiles dir = do
                dirContents <- getDirectoryContents dir
                recursiveGetFiles dir dirContents
