module Hadoo.Pages.Layout where

import Html

base :: Html -> Html
base = Html.doc (Html.title "Hadoo" ++ Html.stylesheet "styles.css" ++ Html.stylesheet "https://fonts.googleapis.com/css2?family=Inter:wght@300;500;700&&display=swap")