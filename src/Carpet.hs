module Carpet(newSierpinskiCarpetFractal) where

import Fractal
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL

-- | Represents a "tile" in the Sierpinsky Carpet fractal
data Tile = Tile
  { x :: Float
  , y :: Float
  , sz :: Float
  }

-- | Subdivides a tile into itself + the 9 subsquares around it
subdivide :: Tile -> [Tile]
subdivide (Tile x y sz)
  = [ Tile (x) (y) sz
    , Tile (x) (y+shift) sz'
    , Tile (x) (y-shift) sz'
    , Tile (x-shift) (y) sz'
    , Tile (x+shift) (y) sz'
    , Tile (x-shift) (y-shift) sz'
    , Tile (x+shift) (y+shift) sz'
    , Tile (x-shift) (y+shift) sz'
    , Tile (x+shift) (y-shift) sz'
    ]
    where shift = sz
          sz' = sz*0.33333

-- | Maps a tile to its OpenGL vertices
-- Note that the position of the tile is the center
vertices :: Tile -> [Float]
vertices (Tile x y sz) =
  [ x-s, y-s, 0.0
  , x-s, y+s, 0.0
  , x+s, y+s, 0.0
  , x+s, y+s, 0.0
  , x+s, y-s, 0.0
  , x-s, y-s, 0.0
  ]
  where s = sz/2

-- | Generates the fractal
sierpinskiCarpet :: Int -> [Tile]
sierpinskiCarpet = (iterate (concatMap subdivide) [Tile 0.0 0.0 0.1] !!)

-- | Generates the fractal's vertices
sierpinskiCarpetVertices :: Int -> V.Vector Float
sierpinskiCarpetVertices n = V.fromList (concatMap vertices (sierpinskiCarpet n))

newSierpinskiCarpetFractal :: Fractal
newSierpinskiCarpetFractal = Fractal GL.Triangles sierpinskiCarpetVertices
