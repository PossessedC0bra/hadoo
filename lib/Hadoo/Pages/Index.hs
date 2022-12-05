module Hadoo.Pages.Index where

import Control.Monad.IO.Class (liftIO)
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
createItem _ (title, text) = Html.div "item" [Html.h2 (show title), text]
