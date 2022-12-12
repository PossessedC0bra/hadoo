module Hadoo.Persistence where

import Hadoo.Enums
import System.Directory
import Text.Printf

initPersistence :: IO ()
initPersistence = do
  mapM_ (createDirectoryIfMissing True . getStateDirectory) (values :: [State])

loadItems :: State -> IO [(Int, String)]
loadItems state = do
  let baseDir = getStateDirectory state
  files <- listDirectory baseDir
  mapM (readItem baseDir) files

readItem :: FilePath -> FilePath -> IO (Int, String)
readItem baseDir filename = do
  let filePath = baseDir ++ filename
  content <- readFile filePath
  return (filenameToId filename, content)

-- MOVE

moveItem :: State -> State -> Int -> IO ()
moveItem origin destination itemId = do
  let oldFile = getStateDirectory origin ++ idToFilename itemId ++ ".txt"
  let newFile = getStateDirectory destination ++ idToFilename itemId ++ ".txt"
  renameFile oldFile newFile

-- DELETE

deleteItem :: State -> Int -> IO ()
deleteItem state itemId = do
  let baseDir = getStateDirectory state
  let filePath = baseDir ++ idToFilename itemId ++ ".txt"
  removeFile filePath

-- UTIL

getStateDirectory :: State -> FilePath
getStateDirectory state = "data/" ++ show state ++ "/"

filenameToId :: String -> Int
filenameToId = read . takeWhile (/= '.')

idToFilename :: Int -> String
idToFilename = printf "%03d"