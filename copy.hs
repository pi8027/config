#!/usr/bin/env runhaskell

module Main where

import Data.Functor
import Control.Monad
import Control.Arrow
import System.FilePath
import System.Directory
import System.Environment

reposPrefix = "dot"
fileList = "filelist"

getFileList :: FilePath -> IO [(String,String)]
getFileList fileName = do
    current <- getCurrentDirectory
    home <- getHomeDirectory
    map (((current</>reposPrefix)++)&&&(home</>)) <$> lines <$> readFile fileName

copyFileOrDirectory :: FilePath -> FilePath -> IO ()
copyFileOrDirectory src dest = do
    putStrLn $ "copy: "++src++" -> "++dest
    isFile <- doesFileExist src
    when isFile $ copyFile src dest
    isDirectory <- doesDirectoryExist src
    when isDirectory $ do
        destExist <- doesDirectoryExist dest
        unless destExist $ createDirectory dest
        map (\n -> (copyFileOrDirectory (src</>n) (dest</>n))) <$>
            filter (not.flip elem [".",".."]) <$> getDirectoryContents src >>= sequence_

main :: IO ()
main = do
    args <- getArgs
    files <- getFileList fileList
    case () of
        _ | elem "export" args -> mapM_ (uncurry copyFileOrDirectory) files
          | elem "import" args -> mapM_ (uncurry (flip copyFileOrDirectory)) files
          | otherwise -> return ()

