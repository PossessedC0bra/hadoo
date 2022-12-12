{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Web where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import qualified Hadoo.Pages.Index
import Hadoo.Persistence
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty

main :: IO ()
main = do
  initPersistence
  scotty 3000 $ do
    middleware logStdoutDev

    get "/" index
    get "/new" index
    post "/items" index
    get "/items/:state/:nr/edit" index
    post "/items/:state/:nr" index
    post "/items/:state/:nr/move/:nextState" $ do
      oldState <- fmap read (param "state")
      nr <- param "nr"
      newState <- fmap read (param "nextState")
      liftIO (moveItem oldState newState nr)
      redirect "/"

    post "/items/:state/:nr/delete" $ do
      state <- fmap read (param "state")
      nr <- param "nr"
      liftIO (deleteItem state nr)
      redirect "/"

    get "/styles.css" styles

-- | HTML
toHtml :: String -> ActionM ()
toHtml = html . LT.pack

index :: ActionM ()
index = do
  indexPage <- liftIO Hadoo.Pages.Index.build
  toHtml indexPage

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
