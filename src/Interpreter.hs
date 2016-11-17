{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Shapes
import Ansi

basicDoc = S.docTypeSvg ! A.version "1.1" ! A.width "50" ! A.height "50" ! A.viewbox "-2 -2 4 4"

interpret :: Drawing -> String
interpret d = renderSvg $ basicDoc $ getSvg d

getSvg :: Drawing -> S.Svg
getSvg d = foldl1 (>>) convertedDrawing
    where convertedDrawing = (map convertShape d)

convertShape :: (Style, Transform, Shape) -> S.Svg
convertShape (st, t, s) = foldl (!) (shapeToSvg s) ((convertStyle st) ++ (convertTransform t))

shapeToSvg :: Shape -> S.Svg
shapeToSvg Empty    = S.rect ! A.width "0" ! A.height "0"
shapeToSvg Square   = S.rect ! A.width "2" ! A.height "2"
shapeToSvg Circle   = S.circle ! A.r "1"

convertStyle :: Style -> [S.Attribute]
convertStyle st = [sw, fc, sc]
    where sw = A.strokeWidth (S.stringValue $ show (strokeWidth st))
          fc = A.fill (S.stringValue $ colourToHex (fillColour st))
          sc = A.stroke (S.stringValue $ colourToHex (strokeColour st))

convertTransform :: Transform -> [S.Attribute]
convertTransform (Compose t1 t2) = (convertTransform t1) ++ (convertTransform t2)
convertTransform Identity = []
convertTransform (Translate (Vector tx ty)) = [A.transform $ S.translate tx ty]
convertTransform (Scale (Vector tx ty)) = [A.transform $ S.scale tx ty]
convertTransform (Rotate m) = [A.transform $ S.matrix m11 m12 0 m21 m22 0]
    where m11 = getX $ getL1 m
          m12 = getY $ getL1 m
          m21 = getX $ getL2 m
          m22 = getY $ getL2 m