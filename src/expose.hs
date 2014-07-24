import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>), splitDirectories)

main :: IO ()
main = do
    args <- getArgs
    currentDir <- getCurrentDirectory
    dirContents <- getDirectoryContents currentDir
    let isRecursive = "-r" `elem` args
    files <- if isRecursive then recursiveGetFiles "" dirContents
                            else getFiles $ map ("" </>) dirContents
    putStrLn . unlines $ files

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
