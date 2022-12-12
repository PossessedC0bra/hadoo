module Hadoo.Pages.EditItem where

import Hadoo.Enums
import Hadoo.Pages.Layout
import Hadoo.Persistence
import Html

build :: State -> Int -> IO Html
build state id = do
  item <- loadItem state id
  return (Hadoo.Pages.Layout.base (Html.div "container" [Html.h1 "Edit Item", Html.form "form" "POST" ("/items/" ++ show state ++ "/" ++ show id) (unwords [Html.label "content" "Content:", contentTextarea item, saveButton])]))

contentTextarea :: (Int, String) -> Html
contentTextarea (_, content) = Html.textarea "content" "content" 12 60 content

saveButton :: Html
saveButton = Html.input "submit" "Save"