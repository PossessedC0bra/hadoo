{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Web where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import qualified Hadoo.Pages.Index
import qualified Hadoo.Pages.NewItem
import Hadoo.Persistence
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty

main :: IO ()
main = do
  initPersistence
  scotty 3000 $ do
    middleware logStdoutDev

    get "/" indexPage
    get "/items" indexPage
    get "/new" newItemPage
    post "/items" createItemAction
    get "/items/:state/:nr/edit" indexPage
    post "/items/:state/:nr/move/:nextState" moveItemAction
    post "/items/:state/:nr/delete" deleteItemAction
    get "/styles.css" styles

-- PAGES

indexPage :: ActionM ()
indexPage = do
  page <- liftIO Hadoo.Pages.Index.build
  toHtml page

newItemPage :: ActionM ()
newItemPage = do
  page <- liftIO Hadoo.Pages.NewItem.build
  toHtml page

-- ACTIONS

createItemAction :: ActionM ()
createItemAction = do
  state <- fmap read (param "state")
  content <- multiLineTextParam "content"
  liftIO (createItem state content)
  redirect "/"

moveItemAction :: ActionM ()
moveItemAction = do
  oldState <- fmap read (param "state")
  nr <- param "nr"
  newState <- fmap read (param "nextState")
  liftIO (moveItem oldState newState nr)
  redirect "/"

deleteItemAction :: ActionM ()
deleteItemAction = do
  state <- fmap read (param "state")
  nr <- param "nr"
  liftIO (deleteItem state nr)
  redirect "/"

-- | HTML
toHtml :: String -> ActionM ()
toHtml = html . LT.pack

demo :: ActionM ()
demo = do
  demoPage <- liftIO (readFile "static/lanes_example.html")
  toHtml demoPage

-- | CSS
styles :: ActionM ()
styles = do
  setHeader "Content-Type" "text/css"
  file "static/styles.css"

-- | Diese Funktion entfernt `\r` Control Characters aus den übertragenen Daten.
-- Sie müssen diese Funktion verwenden um Multiline Textinput ("content") aus einer
-- Textarea auszulesen.
multiLineTextParam :: String -> ActionM String
multiLineTextParam paramName = fmap (filter (/= '\r')) (param (LT.pack paramName))
