module Ansi where

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq,Show,Enum)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Int -> Int -> IO ()
goto x y    = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

color :: Colour -> String -> IO ()
color c s = putStr $ "\ESC[3" ++ show (fromEnum c) ++ "m" ++ s ++ "\ESC[0m"

colourToHex :: Colour -> String
colourToHex Red = "#ff0000"
colourToHex Green = "#00ff00"
colourToHex Blue = "#0000ff"
colourToHex Black = "#000000"
colourToHex White = "#ffffff"
colourToHex Yellow = "#ffff00"
colourToHex Magenta = "#ff00ff"
colourToHex Cyan = "#00ffff"