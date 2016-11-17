{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import Text.Printf
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Shapes
import Ansi

basicDoc = S.docTypeSvg ! A.version "1.1" ! A.width "250" ! A.height "250" ! A.viewbox "-2 -2 4 4"

interpret :: Drawing -> String
interpret d = renderSvg $ basicDoc $ getSvg d

getSvg :: Drawing -> S.Svg
getSvg d = foldl1 (>>) convertedDrawing
    where convertedDrawing = (map convertShape d)

convertShape :: (Style, Transform, Shape) -> S.Svg
convertShape (st, t, s) = foldl (!) (shapeToSvg s) ((convertStyle st) ++ transforms)
    where transforms = [A.transform $ S.toValue $ convertTransform t]

shapeToSvg :: Shape -> S.Svg
shapeToSvg Empty    = S.rect ! A.width "0" ! A.height "0"
shapeToSvg Square   = S.rect ! A.width "2" ! A.height "2"
shapeToSvg Circle   = S.circle ! A.r "1"

convertStyle :: Style -> [S.Attribute]
convertStyle st = [sw, fc, sc]
    where sw = A.strokeWidth (S.stringValue $ show (strokeWidth st))
          fc = A.fill (S.stringValue $ colourToHex (fillColour st))
          sc = A.stroke (S.stringValue $ colourToHex (strokeColour st))

convertTransform :: Transform -> String
convertTransform (Compose t1 t2) = (convertTransform t1) ++ " " ++ (convertTransform t2)
convertTransform Identity = ""
convertTransform (Translate (Vector tx ty)) = printf "translate(%f,%f)" tx ty
convertTransform (Scale (Vector tx ty)) = printf "scale(%f,%f)" tx ty
convertTransform (Rotate m) = printf "matrix(%f,%f,%f,%f,0,0)" m11 m21 m12 m22
    where m11 = getX $ getL1 m
          m12 = getY $ getL1 m
          m21 = getX $ getL2 m
          m22 = getY $ getL2 m