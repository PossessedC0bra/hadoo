module Hadoo.Pages.Layout where

import Html

base :: Html -> Html
base = Html.doc (Html.title "Hadoo" ++ Html.stylesheet "styles.css")