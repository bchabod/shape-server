# ShapeServer

This is the 1st assignment for CS4012 (Topics in Functional Programming). The goal was to develop a Scotty server to produce SVGs from shapes defined in a Haskell DSL.


## Usage

This project has been built using stack. Retrieving dependencies and compiling is pretty syimple:

```bash
stack build
```

And then, to run the server, just type:

```
stack exec shapeserver
```

The server will run on port 3000 (although you can change this in [src/Main.hs](src/Main.hs)). Here are a few examples of inputs:

```h
[(Style {strokeColour = Yellow, fillColour = Magenta, strokeWidth = 0.1},Scale (Vector 1.5 1.5),Circle)]
```

```h
[(Style {strokeColour = Red, fillColour = Blue, strokeWidth = 0.4},Scale (Vector 0.5 0.5),Square)]
```

```h
[(Style {strokeColour = Yellow, fillColour = Magenta, strokeWidth = 0.1},Compose (Scale (Vector 0.75 0.75)) (Translate (Vector (-2.5) (-2.5))),Square), (Style {strokeColour = Yellow, fillColour = Cyan, strokeWidth = 0.3},Compose (Scale (Vector 0.25 0.25)) (Rotate (Matrix (Vector 0.52 (-0.85)) (Vector 0.85 0.52))),Square), (Style {strokeColour = Blue, fillColour = Red, strokeWidth = 0.4},Compose (Scale (Vector 0.25 0.25)) (Translate (Vector 3.0 2.0)),Circle)]
