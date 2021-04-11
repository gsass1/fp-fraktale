module State where

import Data.Maybe
import Carpet
import Chaos
import Hilbert
import Koch
import Fractal
import Sierpinski
import Shader
import qualified Data.Vector.Storable as V

data Resources = Resources
  { mandelShader :: Shader
  , juliaShader :: Shader
  , normalShader :: Shader
  }

data ProgramState = PS
  { fractal :: Int
  , posX :: Double
  , posY :: Double
  , zoom :: Double
  , iterations :: Int
  , exiting :: Bool
  , resources :: Resources
  , fr :: Maybe Fractal
  , verts :: Maybe (V.Vector Float)
  }


-- | Returns the initial program state
initialProgramState :: Resources -> ProgramState
initialProgramState resources' = PS
  { fractal = 0
  , posX = 0.0
  , posY = 0.0
  , zoom = 1.0
  , iterations = 50
  , exiting = False
  , resources = resources'
  , fr = Nothing
  , verts = Nothing
  }

-- | Represents an action that is applied to a ProgramState
data Intent
  = Idle
  | Move Double Double
  | IncreaseZoom
  | DecreaseZoom
  | IncreaseIterations
  | DecreaseIterations
  | SwitchFractal Int
  | Quit

-- | Applies an Intent to a ProgramState resulting in a new ProgramState
applyIntent :: Intent -> ProgramState -> ProgramState
applyIntent (Move dx dy) = moveProgram dx dy
applyIntent IncreaseZoom = increaseZoom
applyIntent DecreaseZoom = decreaseZoom
applyIntent IncreaseIterations = increaseIterations
applyIntent DecreaseIterations = decreaseIterations
applyIntent (SwitchFractal fractalId) = switchFractal fractalId
applyIntent Quit = quitProgram
applyIntent Idle = id

-- | Move the current viewport
moveProgram :: Double -> Double -> ProgramState -> ProgramState
moveProgram dx dy ps = ps { posX = posX' + dx / zoom', posY = posY' + dy / zoom' }
  where posX' = posX ps
        posY' = posY ps
        zoom'  = zoom ps

-- | Increase the current zoom level
increaseZoom :: ProgramState -> ProgramState
increaseZoom ps = ps { zoom = zoom' * 1.2}
  where zoom' = zoom ps

-- | Decrease the current zoom level
decreaseZoom :: ProgramState -> ProgramState
decreaseZoom ps = ps { zoom = zoom' / 1.2}
  where zoom' = zoom ps

-- | Increase the current iteration count
increaseIterations :: ProgramState -> ProgramState
increaseIterations ps = ps { iterations = iterations' + 10}
  where iterations' = iterations ps

-- | Decrease the current iteration count
decreaseIterations :: ProgramState -> ProgramState
decreaseIterations ps = ps { iterations = max (iterations' - 10) 0}
  where iterations' = iterations ps

-- | Changes the current fractal ID and resets iteration count
switchFractal :: Int -> ProgramState -> ProgramState
switchFractal fractalId ps =
  ps { fractal = fractalId
     , fr = fractalIdToFractal fractalId
     , iterations = 50
     }

renderVertices :: ProgramState -> ProgramState
renderVertices ps =
  ps { verts = Just (maybe (Fractal.emptyVertices 0) (flip Fractal.render ((iterations ps) `div` 10)) (fr ps)) }

fractalIdToFractal :: Int -> Maybe Fractal
fractalIdToFractal fractalId =
  case fractalId of
    2 -> Just newSierpinskiCarpetFractal
    3 -> Just newKochFractal
    4 -> Just newSierpinskiTriangleFractal
    5 -> Just newHilbertFractal
    10 -> Just newChaosSierpinskiTriangleFractal 
    11 -> Just newChaosSquareCarpetFractal 
    12 -> Just newChaosVicsekFractal 
    _ -> Nothing

-- | Signals to the main loop that we would like to exit
quitProgram :: ProgramState -> ProgramState
quitProgram ps = ps { exiting = True }

-- | Maps the current fractal ID to its shader
currentFractalShader :: ProgramState -> Shader
currentFractalShader ps =
  case (fractal ps) of
    0 -> (mandelShader $ resources ps)
    1 -> (juliaShader $ resources ps)
    _ -> (normalShader $ resources ps)
