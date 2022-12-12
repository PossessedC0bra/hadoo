module Hadoo.Pages.NewItem where

import Hadoo.Enums
import Hadoo.Pages.Layout
import Html

build :: IO Html
build = do
  return (Hadoo.Pages.Layout.base (Html.div "container" [Html.h1 "New Item", Html.form "form" "POST" "/items" fields]))

fields :: Html
fields = unwords [Html.label "state" "State:", stateDropdown, Html.label "content" "Content:", Html.textarea "content" "content" 12 60, Html.input "submit" "Create"]

stateDropdown :: Html
stateDropdown = Html.select "state" "state" stateOptions

stateOptions :: [(String, String)]
stateOptions = map (\s -> (show s, show s)) (values :: [State])

saveButton :: Html
saveButton = Html.formButton "POST" "/items" "Create"

-- <form method='post' action='/items'>
--   <label for='states'>State: </label>
--   <select name='state' id='states'>
--     <option value='Todo'>Todo</option>
--     <option value='Started'>Started</option>
--   </select>
--   <br>
--   <textarea name='content' rows='12' cols='60'></textarea>
--   <br>
--   <input type='submit' value='Create'></input>
-- </form>