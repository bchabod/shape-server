{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy
import qualified Text.Read as TR

import Shapes
import Ansi
import Interpreter

-- [(Style {strokeColour = Yellow, fillColour = Magenta, strokeWidth = 0.1},Scale (Vector 0.3 0.3),Circle)]

getSVG :: String -> Maybe Drawing
getSVG s = TR.readMaybe s :: Maybe Drawing

getPage :: Maybe Drawing -> String
getPage (Just d) = interpret d
getPage Nothing = "Error! The drawing description that you supplied could not be parsed."

main = scotty 3000 $ do
  get "/" $ file "./static/index.html"
  post "/svg" $ do
	drawing <- (param "drawing") `rescue` (\msg -> return "")
	html $ pack $ getPage $ getSVG $ unpack drawing