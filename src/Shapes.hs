module Shapes(
  Shape(..), Point, Vector(..), Transform(..), Style(..), Drawing,
  point, getX, getY, getL1, getL2,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>),
  inside, getColour)  where

import Ansi
import Data.Maybe

-- Utilities

data Vector = Vector Double Double
              deriving (Show, Read)
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving (Show, Read)

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y
getL1 (Matrix v1 v2) = v1
getL2 (Matrix v1 v2) = v2

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty 
           | Circle 
           | Square
             deriving (Show, Read)

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving (Show, Read)

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Stylesheet

data Style = Style {strokeColour :: Colour, fillColour :: Colour, strokeWidth :: Double} deriving (Show, Read)

-- Drawings

type Drawing = [(Style,Transform,Shape)]

-- interpretation function for drawings

getColour :: Point -> Drawing -> Colour
getColour p d = head $ mapMaybe (inside2 p) d

inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Style, Transform, Shape) -> Bool
inside1 p (st,t,s) = insides (transform t p) s

inside2 :: Point -> (Style, Transform, Shape) -> Maybe Colour
inside2 p (st,t,s) = if isInside
                     then if ifInsideBorder
                        then Just (fillColour st)
                        else Just (strokeColour st)
                     else Nothing
  where isInside = insides (transform t p) s
        ifInsideBorder = insidesBorder (transform t p) s (strokeWidth st)

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1

insidesBorder :: Point -> Shape -> Double -> Bool
insidesBorder p Empty _ = False
insidesBorder p Circle strokeWidth = distance p <= 1 - strokeWidth
insidesBorder p Square strokeWidth = maxnorm  p <= 1 - strokeWidth

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 10), circle)
