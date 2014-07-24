import Control.Monad (filterM)
import System.Directory (doesFileExist, getCurrentDirectory,
                         getDirectoryContents)

main :: IO ()
main = do
    cd <- getCurrentDirectory
    dirContents <- getDirectoryContents cd
    files <- filterM doesFileExist dirContents
    putStrLn . unwords $ files
