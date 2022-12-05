module Hadoo.Persistence where

import Hadoo.Enums
import System.Directory

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
  return (extractId filename, content)

extractId :: String -> Int
extractId = read . takeWhile (/= '.')

getStateDirectory :: State -> FilePath
getStateDirectory state = "data/" ++ show state ++ "/"