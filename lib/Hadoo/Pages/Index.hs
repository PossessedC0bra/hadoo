module Hadoo.Pages.Index where

import Hadoo.Enums
import Hadoo.Pages.Layout
import Hadoo.Persistence
import Html

build :: IO Html
build = do
  lanes <- mapM lane (values :: [State])
  return (Hadoo.Pages.Layout.base (Html.div "container" [Html.h1 "Hadoo", newItemButton, Html.div "lanes" lanes]))

newItemButton :: Html
newItemButton = formButton "GET" "/new" "New Item"

lane :: State -> IO Html
lane state = do
  items <- loadItems state
  let htmlItems = map (item state) items
  return (Html.div "lane" (Html.h2 (show state ++ " (" ++ show (length htmlItems) ++ ")") : htmlItems))

item :: State -> (Int, String) -> Html
item state (id, text) = Html.div "item" (Html.pre text : itemButtons state id)

itemButtons :: State -> Int -> [Html]
itemButtons state id
  | state == minBound = [moveRightButton state id, editButton state id, deleteButton state id]
  | state == maxBound = [moveLeftButton state id, editButton state id, deleteButton state id]
  | otherwise = [moveLeftButton state id, moveRightButton state id, editButton state id, deleteButton state id]

moveLeftButton :: State -> Int -> Html
moveLeftButton state id = formButton "POST" ("/items/" ++ show state ++ "/" ++ show id ++ "/move/" ++ show (pred state)) "<"

moveRightButton :: State -> Int -> Html
moveRightButton state id = formButton "POST" ("/items/" ++ show state ++ "/" ++ show id ++ "/move/" ++ show (succ state)) ">"

editButton :: State -> Int -> Html
editButton state id = formButton "GET" ("/items/" ++ show state ++ "/" ++ show id ++ "/edit") "Edit"

deleteButton :: State -> Int -> Html
deleteButton state id = formButton "POST" ("/items/" ++ show state ++ "/" ++ show id ++ "/delete") "Delete"
