module Main where

import Cueparse
import System.Environment

main :: IO ()
main = do
  filePath <- getArgs;
  fileContents <- readFile $ head filePath
  mapM_ print $ parseCuesheet fileContents
