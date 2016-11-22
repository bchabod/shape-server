{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy
import qualified Text.Read as TR

import Shapes
import Ansi
import Interpreter

-- A few examples to test:
-- Basic circle is [(Style {strokeColour = Yellow, fillColour = Magenta, strokeWidth = 0.1},Scale (Vector 1.5 1.5),Circle)]
-- Basic square is [(Style {strokeColour = Red, fillColour = Blue, strokeWidth = 0.4},Scale (Vector 0.5 0.5),Square)]
-- Complicated is [(Style {strokeColour = Yellow, fillColour = Magenta, strokeWidth = 0.1},Compose (Scale (Vector 0.75 0.75)) (Translate (Vector (-2.5) (-2.5))),Square), (Style {strokeColour = Yellow, fillColour = Cyan, strokeWidth = 0.3},Compose (Scale (Vector 0.25 0.25)) (Rotate (Matrix (Vector 0.52 (-0.85)) (Vector 0.85 0.52))),Square), (Style {strokeColour = Blue, fillColour = Red, strokeWidth = 0.4},Compose (Scale (Vector 0.25 0.25)) (Translate (Vector 3.0 2.0)),Circle)]

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