module Html where

import Control.Monad.Trans.Select (Select)
import Data.List (intersperse)

-- | Type Alias für Html Strings
type Html = String

doc :: Html -> Html -> Html
doc headContent bodyContent = "<!DOCTYPE html>" ++ ea "html" [("lang", "en")] (Html.head headContent ++ Html.body bodyContent)

head :: Html -> Html
head = e "head"

title :: String -> Html
title = e "title"

link :: String -> String -> Html
link rel href = ea "link" [("rel", rel), ("href", href)] ""

stylesheet :: String -> Html
stylesheet = link "stylesheet"

body :: Html -> Html
body = e "body"

div :: String -> [Html] -> Html
div clazz = ea "div" [("class", clazz)] . concat

pre :: Html -> Html
pre = e "pre"

form :: String -> String -> String -> Html -> Html
form clazz method action = eaClass "form" clazz [("method", method), ("action", action)]

label :: String -> Html -> Html
label for = ea "label" [("for", for)]

formField :: String -> String -> String -> [(String, String)] -> Html -> Html
formField tag id name attrs = ea tag (("class", "form-field") : ("id", id) : ("name", name) : attrs)

select :: String -> String -> [(String, String)] -> Html
select id name options = formField "select" id name [] (concatMap option options)

option :: (String, String) -> Html
option (value, text) = ea "option" [("value", value)] text

textarea :: String -> String -> Int -> Int -> String -> Html
textarea id name rows cols = formField "textarea" id name [("rows", show rows), ("cols", show cols)]

input :: String -> String -> Html
input submitType value = ea "input" [("type", submitType), ("value", value)] ""

button :: Html -> Html
button = ea "button" [("type", "submit")]

formButton :: String -> String -> String -> Html
formButton method action text = Html.form "inline" method action (Html.button text)

h1 :: Html -> Html
h1 = e "h1"

h2 :: Html -> Html
h2 = e "h2"

-- ELEMENT UTILITIES

e :: String -> Html -> Html
e tag = ea tag []

eClass :: String -> String -> Html -> Html
eClass tag className = ea tag [("class", className)]

eaClass :: String -> String -> [(String, String)] -> Html -> Html
eaClass tag className attrs = ea tag (("class", className) : attrs)

ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where
    attrsHtml [] = []
    attrsHtml as = " " : intersperse " " (map attrHtml as)
    attrHtml (key, value) = key ++ "='" ++ value ++ "'"