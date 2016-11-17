module Example where

import Shapes
import Ansi
import Signal
import Animate (animate)
import Render (defaultWindow)
import Interpreter

staticBall :: Signal Drawing
staticBall = pure [(Style {strokeColour=Red, fillColour=Red, strokeWidth=1}, scale (point 0.5 0.5) <+> translate (point 1.2 0.4), circle)]

addT :: (Style, Transform, Shape) -> Transform -> (Style, Transform, Shape)
addT (st,ts,s) t = (st, ts <+> t, s)

preaddT :: (Style, Transform, Shape) -> Transform -> (Style, Transform, Shape)
preaddT (st,ts,s) t = (st, t <+> ts, s)

rotatingSquare :: Signal Drawing
rotatingSquare = fmap (:[]) $ fmap (addT sq) rs 
     where            
           rs :: Signal Transform
           rs = fmap rotate timeS -- using timeS as the source for the rotation angle

           sq :: (Style, Transform, Shape)
           sq = ( Style {strokeColour=Red, fillColour=Green, strokeWidth=0.1}, scale (point 0.5 0.5) <+> translate (point 1.2 0.4) , square)

bouncingBall :: Signal Drawing
bouncingBall = fmap (:[]) $ fmap (preaddT ball) ( fmap translate pos )
       where bounceY = fmap (sin . (3*)) timeS
             bounceX = fmap (sin . (2*)) timeS
             pos = pure point <*> bounceX <*> bounceY
             ball = ( Style {strokeColour=Yellow, fillColour=Magenta, strokeWidth=0.1}, scale (point 0.3 0.3), circle )




movingBall :: Signal Drawing
movingBall = fmap (:[]) $ fmap (addT ball) ts
       where
             ts :: Signal Transform
             ts = fmap translate pos

             bounceY :: Signal Double
             bounceY = fmap (sin . (3*)) timeS

             pos :: Signal Point
             pos = pure point <*> pure 0.0 <*> bounceY

             ball :: (Style, Transform, Shape)
             ball = ( Style {strokeColour=Yellow, fillColour=Magenta, strokeWidth=0.1}, scale (point 0.3 0.3), circle )

joinDS :: Signal [a] -> Signal [a] -> Signal [a]
joinDS s0 s1 = (fmap ( (++) ) s0) <*> s1


--example = staticBall
example = bouncingBall `joinDS` rotatingSquare

           
runExample :: IO ()
runExample = do
  putStrLn svg
    where svg = interpret [( Style {strokeColour=Yellow, fillColour=Magenta, strokeWidth=0.1}, scale (point 0.3 0.3), circle )]

