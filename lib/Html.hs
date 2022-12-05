{-# LANGUAGE OverloadedStrings #-}

module Html where

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

h1 :: Html -> Html
h1 = e "h1"

h2 :: Html -> Html
h2 = e "h2"

-- | Erzeugt ein Element ohne Attribute
e :: String -> Html -> Html
e tag = ea tag []

-- | Erzeugt ein Element mit klassennamen
eClass :: String -> String -> Html -> Html
eClass tag className = ea tag [("class", className)]

-- | Erzeugt ein Element mit Attributen
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where
    attrsHtml [] = []
    attrsHtml as = " " : intersperse " " (map attrHtml as)
    attrHtml (key, value) = key ++ "='" ++ value ++ "'"