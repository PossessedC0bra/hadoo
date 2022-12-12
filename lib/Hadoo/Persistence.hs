module Hadoo.Persistence where

import Hadoo.Enums
import System.Directory
import Text.Printf

initPersistence :: IO ()
initPersistence = do
  mapM_ (createDirectoryIfMissing True . getStateDirectory) (values :: [State])

-- CREATE

createItem :: State -> String -> IO ()
createItem state content = do
  newId <- findNextId state
  let filePath = getStateDirectory state ++ idToFilename newId ++ ".txt"
  putStrLn filePath
  writeFile filePath content

-- READ

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
  newId <- findNextId destination
  let newFile = getStateDirectory destination ++ idToFilename newId ++ ".txt"
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

findNextId :: State -> IO Int
findNextId state = do
  let baseDir = getStateDirectory state
  files <- listDirectory baseDir
  let ids = -1 : map filenameToId files
  return (maximum ids + 1)