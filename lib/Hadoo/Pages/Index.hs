module Hadoo.Pages.Index where

import Hadoo.Enums
import Hadoo.Pages.Layout
import Hadoo.Persistence
import Html

build :: IO Html
build = do
  lanes <- mapM createLane (values :: [State])
  return (Hadoo.Pages.Layout.base (Html.div "" [Html.h1 "Hadoo", Html.div "container" lanes]))

createLane :: State -> IO Html
createLane state = do
  items <- loadItems state
  let htmlItems = map (createItem state) items
  return (Html.div "lane" (Html.h2 (show state ++ " (" ++ show (length htmlItems) ++ ")") : htmlItems))

createItem :: State -> (Int, String) -> Html
createItem state (id, text) = Html.div "item" [Html.pre text, createItemButtons state id]

createItemButtons :: State -> Int -> Html
createItemButtons state id = createItemButton "POST" ("/items/" ++ show state ++ "/" ++ show id ++ "/delete") "Delete"

createItemButton :: String -> String -> String -> Html
createItemButton method action text = Html.form "inline" method action (Html.button text)
